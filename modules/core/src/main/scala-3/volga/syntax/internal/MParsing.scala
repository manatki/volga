package volga.syntax.internal

import scala.{PartialFunction as =\>}
import scala.quoted.Quotes

trait MParsing[q <: Quotes](val q: q):
    import q.reflect.*

    private type Mid      = STerm[Pos.Mid, String, Tree]
    private type End      = STerm[Pos.End, String, Tree]
    private type Anywhere = STerm[Any, String, Tree]
    private type App      = STerm.Application[String, Tree]

    val asMidSTerm: Tree =\> Mid = {
        case asAnywhereTerm(t)           => t
        case ValDef(name, _, Some(expr)) => STerm.Assignment(Vector(name), STerm.Application(expr, Vector()))
    }

    val asEndTerm: Tree =\> End = =\>.empty

    val asApplication: Tree =\> App = { case Apply(t, List(Apply(_, List(vars)))) =>
        STerm.Application(t, Vector())
    }

    val asAnywhereTerm: Tree =\> Anywhere = asApplication
end MParsing
