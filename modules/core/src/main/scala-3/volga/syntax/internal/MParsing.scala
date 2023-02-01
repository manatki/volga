package volga.syntax.internal

import scala.{PartialFunction as =\>}
import scala.quoted.{Quotes, Type}
import scala.annotation.threadUnsafe
import scala.annotation.tailrec
import volga.free.Nat.Vec

final class MParsing[q <: Quotes & Singleton](using val q: q):
    import q.reflect.*
    import Pos.*

    private type Mid      = STerm[Pos.Mid, String, Tree]
    private type MidT     = STerm[Pos.Mid & Pos.Tupling, String, Tree]
    private type End      = STerm[Pos.End, String, Tree]
    private type Anywhere = STerm[Any, String, Tree]
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
            report.info(s"${t.tpe.show}: ${t.tpe.classSymbol}")
            STerm.Tupled(name, 0, app)
        case ValDef(name, _, Some(Select(Ident(tname), s"_${OfInt(i)}")))                       =>
            STerm.Untupling(name, tname, i)
    end asMidSTerm

    val asEndTerm: Tree =\> End =
        case Typed(Ident(name), _)                    => STerm.Result(Vector(name))
        case Apply(TupleApp(()), ident.travector(names)) => STerm.Result(names)

    val asApplication: Tree =\> App =
        case Apply(Apply(_, List(t)), ident.travector(names)) =>
            STerm.Application(t, names)

    val ident: Term =\> String =
        case Ident(name) => name

    val TupleApp: Term =\> Unit =
        case TypeApply(Select(Ident(s"Tuple$_"), "apply"), _) =>

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

    val asAnywhereTerm: Tree =\> Anywhere = asApplication

end MParsing
