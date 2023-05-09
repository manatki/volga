package volga.syntax.parsing

import scala.quoted.Quotes
import volga.syntax.solve.StageList
import scala.quoted.Expr

final class MGeneration[q <: Quotes & Singleton](using val q: q)(using typing: MonadicTyping[q.type]):
    import q.reflect.*

    def generate(terms: StageList.Adapted[Tree, Var[q.type], TypeRepr]): Tree =
        ???
