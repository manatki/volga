package volga.syntax.internal

import scala.{PartialFunction as =\>}
import scala.quoted.Quotes

final class MParsing[q <: Quotes & Singleton](val q: q):
    import q.reflect.*

    private type Mid      = STerm[Pos.Mid, String, Tree]
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

    val asMidSTerm: Tree =\> Mid =
        case asAnywhereTerm(t)           => t
        case ValDef(name, _, Some(expr)) =>
            STerm.Assignment(Vector(name), STerm.Application(expr, Vector()))

    val asEndTerm: Tree =\> End = =\>.empty

    val asApplication: Tree =\> App =
        case Apply(t, List(Apply(_, List(vars)))) =>
            STerm.Application(t, Vector())
        
    val asAnywhereTerm: Tree =\> Anywhere = asApplication
end MParsing
