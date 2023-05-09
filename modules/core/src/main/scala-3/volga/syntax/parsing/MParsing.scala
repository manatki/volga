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
import volga.syntax.smc.SyApp

final class MParsing[q <: Quotes & Singleton](using val q: q):
    import q.reflect.*
    import Pos.*
    val vars = Vars[q.type]()

    val SyAppRepr = TypeRepr.of[SyApp[?, ?]]

    type Parsed = (Vector[Var[q.type]], Vector[PMid], PEnd)

    @tailrec def parseBlock(block: Tree): Either[VError, Parsed] = block match
        case CFBlock(body)          => parseBlock(body)
        case Block(Nil, res: Block) => parseBlock(res)
        case InlineTerm(t)          => parseBlock(t)
        case Lambda(valDefs, body)  => parseWithVals(valDefs, body)
        case body                   => parseWithVals(Nil, body)

    private def parseWithVals(valDefs: List[ValDef], block: Tree): Either[VError, Parsed] = block match
        case Block(mids, res) =>
            val vs = valDefs.toVector.map:
                case ValDef(name, tpe, _) => vars.varOf(name, tpe)
            for
                midTerms     <- VError.traverse(mids, asMidSTerm)(midTermErr)
                detupledMids <- detuple(midTerms)
                endTerm      <- VError.applyOr(res)(asEndTerm)(endTermErr(res))
            yield 
                (vs, detupledMids, endTerm)

    private type PMid      = STerm[Var[q], Tree] & Pos.Mid
    private type PMidTup   = STerm[Var[q], Tree] & (Pos.Mid | Pos.Tupling)
    private type PEnd      = STerm[Var[q], Tree] & Pos.End
    private type PAnywhere = STerm[Var[q], Tree] & Pos.Mid & Pos.End
    private type PApp      = App[Var[q], Tree]

    val InlineTerm: Inlined =\> Term =
        case Inlined(_, _, t) => t

    private val CFBlock: Tree =\> Tree =
        case Lambda(List(p), t) if p.tpt.tpe <:< SyAppRepr => t

    val asMidSTerm: Tree =\> PMidTup =
        case asAnywhereTerm(t)                                                                  => t
        case ValDef(name, t, Some(asApplication(app)))                                          =>
            STerm.Assignment(Vector(vars.varOf(name, t)), app)
        case ValDef(name, _, Some(Match(Typed(asApplication(app), t @ TupleRepr(i)), List(_)))) =>
            STerm.Tupled(vars.varNamed(name), i, app)
        case ValDef(name, t, Some(Select(Ident(tname), s"_${OfInt(i)}")))                       =>
            STerm.Untupling(vars.varNamed(tname), vars.varOf(name, t), i - 1)
    end asMidSTerm

    val asEndTerm: Tree =\> PEnd =
        case asAnywhereTerm(t)                           => t
        case Typed(Ident(name), _)                       => STerm.Result(Vector(vars.varNamed(name)))
        case Apply(TupleApp(()), ident.travector(names)) => STerm.Result(names.map(vars.varNamed))

    val asApplication: Tree =\> PApp =
        case Apply(Apply(_, List(t)), ident.travector(names)) =>
            App(t, names.map(vars.varNamed))

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

    case class TuplingState(app: Option[PApp] = None, bindings: Vector[Option[Var[q]]] = Vector.empty, arity: Int = 0):
        def addBinding(index: Int, binding: Var[q]) =
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

        private def updateTupling(v: Var[q])(f: TuplingState => TuplingState) =
            f(tuplings.getOrElse(v.name, TuplingState())) match
                case CompleteTuplingState(mid) => add(mid).copy(tuplings = tuplings - v.name)
                case tupling                   => copy(tuplings = tuplings.updated(v.name, tupling))

        def push(cmd: STerm[Var[q], Tree] & (Pos.Mid | Pos.Tupling)): DetupleState = cmd match
            case STerm.Tupled(receiver, arity, application) => updateTupling(receiver)(_.define(application, arity))
            case STerm.Untupling(src, tgt, index)           => updateTupling(src)(_.addBinding(index, tgt))
            case term: Pos.Mid                              => add(term)

        def end: Either[VError, Vector[PMid]] =
            if tuplings.isEmpty then Right(result)
            else
                val errs = tuplings.values.collect:
                    case TuplingState(Some(App(tree, _)), _, _) =>
                        VError.atTree(tree)("incorrect tupling")

                Left(errs.reduce(_ ++ _))

    end DetupleState

    def detuple(commands: Iterable[PMidTup], acc: Vector[PMid] = Vector.empty)(using
        Quotes
    ): Either[VError, Vector[PMid]] =
        commands.foldLeft(DetupleState())(_.push(_)).end

end MParsing
