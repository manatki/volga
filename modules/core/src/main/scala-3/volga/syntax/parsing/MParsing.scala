package volga.syntax.parsing

import scala.{PartialFunction as =\>}
import scala.quoted.{Quotes, Type}
import scala.annotation.threadUnsafe
import scala.annotation.tailrec
import volga.free.Nat.Vec

import volga.syntax.parsing.VError

import volga.syntax.parsing.Pos.{Mid, End, Tupling}
import volga.syntax.parsing.STerm
import volga.syntax.parsing.App

final class MParsing[q <: Quotes & Singleton](using val q: q):
    import q.reflect.*
    import Pos.*

    case class Var(name: String, typ: Option[TypeRepr] = None):
        override def toString: String = typ match
            case None    => name
            case Some(t) => s"$name: ${t.show}"

    object Var:
        def of(name: String, t: TypeTree): Var =
            t.tpe match
                case AppliedType(t1, List(t2)) => Var(name, Some(simp(t2)))
                case _                         => Var(name)

        private def simp(t: TypeRepr): TypeRepr = t match
            case AppliedType(t, ts) => AppliedType(t.dealias, ts.map(simp))
            case t                  => t.dealias
    end Var

    private type PMid      = STerm[Var, Tree] & Pos.Mid
    private type PMidTup   = STerm[Var, Tree] & (Pos.Mid | Pos.Tupling)
    private type PEnd      = STerm[Var, Tree] & Pos.End
    private type PAnywhere = STerm[Var, Tree] & Pos.Mid & Pos.End
    private type PApp      = App[Var, Tree]

    val InlineTerm: Inlined =\> Term =
        case Inlined(None, Nil, t) => t

    val CFBody: Term =\> Term =
        case Block(Nil, t) => t
        case t             => t

    val CFBlock: Tree =\> Tree =
        case InlineTerm(
              Block(
                List(DefDef(nameD, _, _, Some(InlineTerm(CFBody(t))))),
                Closure(Ident(nameR), None)
              )
            ) if nameD == nameR =>
            t
    end CFBlock

    val asMidSTerm: Tree =\> PMidTup =
        case asAnywhereTerm(t)                                                                  => t
        case ValDef(name, t, Some(asApplication(app)))                                          =>
            STerm.Assignment(Vector(Var.of(name, t)), app)
        case ValDef(name, _, Some(Match(Typed(asApplication(app), t @ TupleRepr(i)), List(_)))) =>
            STerm.Tupled(Var(name), i, app)
        case ValDef(name, t, Some(Select(Ident(tname), s"_${OfInt(i)}")))                       =>
            STerm.Untupling(Var(tname), Var.of(name, t), i - 1)
    end asMidSTerm

    val asEndTerm: Tree =\> PEnd =
        case Typed(Ident(name), _)                       => STerm.Result(Vector(Var(name)))
        case Apply(TupleApp(()), ident.travector(names)) => STerm.Result(names.map(Var(_)))

    val asApplication: Tree =\> PApp =
        case Apply(Apply(_, List(t)), ident.travector(names)) =>
            App(t, names.map(Var(_)))

    val ident: Term =\> String =
        case Ident(name) => name

    val TupleApp: Term =\> Unit =
        case TypeApply(Select(Ident(s"Tuple$_"), "apply"), _) =>

    val asAnywhereTerm: Tree =\> PAnywhere =
        case asApplication(app) => STerm.Application(app)
    object TupleRepr:
        @threadUnsafe lazy val ConsS = TypeRepr.of[? *: ?].classSymbol
        @threadUnsafe lazy val NilS  = TypeRepr.of[EmptyTuple].classSymbol

        def unapply(x: TypeTree): Option[Int] =
            val t = x.tpe.dealias
            if t.isTupleN then Some(t.typeArgs.size)
            else arity(t, 0, x.pos)

        @tailrec private def arity(tr: TypeRepr, acc: Int, pos: Position): Option[Int] =
            tr.classSymbol match
                case ConsS =>
                    tr.typeArgs match
                        case List(_, tail) => arity(tail, acc + 1, pos)
                        case args          =>
                            report.error("unexpected error while matching tuples", pos)
                            None

                case NilS => Some(acc)
                case _    => None
    end TupleRepr

    val midTermErr = VError.atTree(_: Tree)("error while parsing mid term")
    val endTermErr = VError.atTree(_: Tree)("error while parsing end term")

    private def fullVector[A] = ({ case Some(a) => a }: Option[A] =\> A).travector

    case class TuplingState(app: Option[PApp] = None, bindings: Vector[Option[Var]] = Vector.empty, arity: Int = 0):
        def addBinding(index: Int, binding: Var) =
            val newBindings =
                if bindings.size > index then bindings.updated(index, Some(binding))
                else bindings ++ Vector.fill(index - bindings.size)(None) :+ Some(binding)
            copy(bindings = newBindings)

        def define(app: PApp, arity: Int) = copy(app = Some(app), arity = arity)

    val CompleteTuplingState: TuplingState =\> PMid =
        case TuplingState(Some(app), fullVector(xs), arity) if arity > 0 && xs.size == arity =>
            STerm.Assignment(xs, app)

    case class DetupleState(
        tuplings: Map[String, TuplingState] = Map.empty,
        result: Vector[PMid] = Vector.empty
    ):
        private def add(mid: PMid) = copy(result = result :+ mid)

        private def updateTupling(v: Var)(f: TuplingState => TuplingState) =
            f(tuplings.getOrElse(v.name, TuplingState())) match
                case CompleteTuplingState(mid) => add(mid).copy(tuplings = tuplings - v.name)
                case tupling                   => copy(tuplings = tuplings.updated(v.name, tupling))

        def push(cmd: STerm[Var, Tree] & (Pos.Mid | Pos.Tupling)): DetupleState = cmd match
            case STerm.Tupled(receiver, arity, application) => updateTupling(receiver)(_.define(application, arity))
            case STerm.Untupling(src, tgt, index)           => updateTupling(src)(_.addBinding(index, tgt))
            case term: Pos.Mid                              => add(term)

        def end: Either[VError, Vector[PMid]] =
            if tuplings.isEmpty then Right(result)
            else
                Left(() =>
                    tuplings.values.foreach {
                        case TuplingState(Some(App(tree, _)), _, _) =>
                            q.reflect.report.error("incorrect tupling", tree.pos)
                        case _                                      =>
                    }
                )

    end DetupleState

    def detuple(commands: Iterable[PMidTup], acc: Vector[PMid] = Vector.empty)(using
        Quotes
    ): Either[VError, Vector[PMid]] =
        commands.foldLeft(DetupleState())(_.push(_)).end

end MParsing
