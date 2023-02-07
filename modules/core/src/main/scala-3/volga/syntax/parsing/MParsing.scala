package volga.syntax.parsing

import scala.{PartialFunction as =\>}
import scala.quoted.{Quotes, Type}
import scala.annotation.threadUnsafe
import scala.annotation.tailrec
import volga.free.Nat.Vec

import volga.syntax.parsing.VError

import volga.syntax.parsing.Pos.{Mid, End, Tupling}
import volga.syntax.parsing.STerm

final class MParsing[q <: Quotes & Singleton](using val q: q):
    import q.reflect.*
    import Pos.*

    private type Mid      = STerm[String, Tree] & Pos.Mid
    private type MidT     = STerm[String, Tree] & (Pos.Mid | Pos.Tupling)
    private type End      = STerm[String, Tree] & Pos.End
    private type Anywhere = STerm[String, Tree] & Pos.Mid & Pos.End
    private type App      = STerm.Application[String, Tree]

    val InlineTerm: Inlined =\> Term =
        case Inlined(None, Nil, t) => t

    val CFBlock: Tree =\> Tree =
        case InlineTerm(
              Block(
                List(DefDef(nameD, _, _, Some(InlineTerm(Block(Nil, t))))),
                Closure(Ident(nameR), None)
              )
            ) if nameD == nameR =>
            t
    end CFBlock

    val asMidSTerm: Tree =\> MidT =
        case asAnywhereTerm(t)                                                                  => t
        case ValDef(name, _, Some(asApplication(app)))                                          =>
            STerm.Assignment(Vector(name), app)
        case ValDef(name, _, Some(Match(Typed(asApplication(app), t @ TupleRepr(i)), List(_)))) =>
            STerm.Tupled(name, i, app)
        case ValDef(name, _, Some(Select(Ident(tname), s"_${OfInt(i)}")))                       =>
            STerm.Untupling(tname, name, i - 1)
    end asMidSTerm

    val asEndTerm: Tree =\> End =
        case Typed(Ident(name), _)                       => STerm.Result(Vector(name))
        case Apply(TupleApp(()), ident.travector(names)) => STerm.Result(names)

    val asApplication: Tree =\> App =
        case Apply(Apply(_, List(t)), ident.travector(names)) =>
            STerm.Application(t, names)

    val ident: Term =\> String =
        case Ident(name) => name

    val TupleApp: Term =\> Unit =
        case TypeApply(Select(Ident(s"Tuple$_"), "apply"), _) =>

    val asAnywhereTerm: Tree =\> Anywhere = asApplication
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

    case class TuplingState(app: Option[App] = None, bindings: Vector[Option[String]] = Vector.empty, arity: Int = 0):
        def addBinding(index: Int, binding: String) =
            val newBindings =
                if bindings.size > index then bindings.updated(index, Some(binding))
                else bindings ++ Vector.fill(index - bindings.size)(None) :+ Some(binding)
            copy(bindings = newBindings)

        def define(app: App, arity: Int) = copy(app = Some(app), arity = arity)

    val CompleteTuplingState: TuplingState =\> Mid =
        case TuplingState(Some(app), fullVector(xs), arity) if arity > 0 && xs.size == arity =>
            STerm.Assignment(xs, app)

    case class DetupleState(
        tuplings: Map[String, TuplingState] = Map.empty,
        result: Vector[Mid] = Vector.empty
    ):
        private def add(mid: Mid) = copy(result = result :+ mid)

        private def updateTupling(name: String)(f: TuplingState => TuplingState) =
            f(tuplings.getOrElse(name, TuplingState())) match
                case CompleteTuplingState(mid) => add(mid).copy(tuplings = tuplings - name)
                case tupling                   => copy(tuplings = tuplings.updated(name, tupling))

        def push(cmd: STerm[String, Tree] & (Pos.Mid | Pos.Tupling)): DetupleState = cmd match
            case STerm.Tupled(receiver, arity, application) => updateTupling(receiver)(_.define(application, arity))
            case STerm.Untupling(src, tgt, index)           => updateTupling(src)(_.addBinding(index, tgt))
            case term: Pos.Mid                              => add(term)

        def end: Either[VError, Vector[Mid]] =
            if tuplings.isEmpty then Right(result)
            else
                Left(() =>
                    tuplings.values.foreach {
                        case TuplingState(Some(STerm.Application(tree, _)), _, _) =>
                            q.reflect.report.error("incorrect tupling", tree.pos)
                        case _                                                    =>
                    }
                )

    end DetupleState

    def detuple(commands: Iterable[MidT], acc: Vector[Mid] = Vector.empty)(using Quotes): Either[VError, Vector[Mid]] =
        commands.foldLeft(DetupleState())(_.push(_)).end

end MParsing
