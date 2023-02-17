package volga.syntax.parsing

import scala.quoted.Quotes

final class MGeneration[q <: Quotes & Singleton](using val q: q)(using typing: MonadicTyping[q.type]):
    import q.reflect.*

    def generate(terms: StageList.Vars[Tree, Var[q.type]]): Tree =
        val adaptations = StageList.withAdaptation(terms)
        ???
