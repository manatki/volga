package volga.syntax.parsing

import scala.quoted.Quotes
import scala.collection.View.Single
import volga.syntax.solve.PMagma

trait MonadicTyping[q <: Quotes](using val q: q):
    import q.reflect.*
    def one: TypeRepr
    def tensor: TypeRepr

object MndType:
    opaque type MndType[+q, TR] <: TR = TR

    def apply[TR](q: TR): MndType[Nothing, TR] = q

    given [q <: Quotes & Singleton](using q: q, typing: MonadicTyping[q.type]): PMagma[MndType[q, q.reflect.TypeRepr]] =
        import q.reflect.*
        new:
            def empty                                = typing.one
            extension (x: MndType[q, TypeRepr])
                def combine(y: MndType[q, TypeRepr]) = AppliedType(typing.tensor, List(x, y))
end MndType

type MndType[q <: Quotes & Singleton, TR] = MndType.MndType[q, TR]
