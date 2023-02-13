package volga.syntax.parsing

import scala.quoted.Quotes

final class MGeneration[q <: Quotes & Singleton](using val q: q)(using typing: MonadicTyping[q.type]):
    import q.reflect.*

    def generate(terms: StageList[Tree, Vector[Var[q.type]], Vector[Var[q.type]]]): Tree =
        val adaptations = terms.withAdaptation[String, MndType[q, TypeRepr]](_.mndType, _.name)
        ???
