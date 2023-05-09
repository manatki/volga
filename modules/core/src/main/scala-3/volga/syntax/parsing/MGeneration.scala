package volga
package syntax.parsing

import scala.quoted.Quotes
import volga.syntax.solve.StageList
import scala.quoted.Expr
import scala.quoted.Type
import volga.syntax.solve.StageList.VarList

final class MGeneration[H[_, _], U[_], q <: Quotes & Singleton](sym: Expr[SymmetricCat[H, U]])(using val q: q)(using
    typing: MonadicTyping[q.type]
):
    import q.reflect.*

    def generate(terms: StageList.Adapted[Var[q.type], Tree, MndType[q, TypeRepr]]): Tree =
        val untypedNames = terms.view
            .map(_.next)
            .flatMap(x => x.effective ++ x.goThrough)
            .collect { case v if v.typ.isEmpty => v.name }
            .mkString(", ")
        if untypedNames.nonEmpty then report.warning(s"Untyped variables: $untypedNames")
        '{ () }.asTerm
    end generate

    private def genType(v: VarList[Var[q.type]]): TypeRepr = ???

    private def joinType(v: Vector[Var[q.type]]): TypeRepr = ???

    private def generateTerm[A: Type](adapt: StageList.Adapt[Tree, Var[q.type], TypeRepr]): Tree = ???

    private def compose(a: Tree, b: Tree): Tree = ???
end MGeneration
