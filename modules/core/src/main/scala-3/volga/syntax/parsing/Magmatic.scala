package volga.syntax.parsing

import volga.syntax.solve.PMagma

trait Labeled[V]:
    type Label
    def label(v: V): Label

    def toMap(vs: Vector[V]): Map[Label, V] =
        vs.map(v => label(v) -> v).toMap

trait Variable[V, Description](using val describeMagma: PMagma[Description]) extends Labeled[V]:
    def describe(v: V): Description

object Variable:

    private given PMagma[String] with
        def empty: String                                    = ""
        extension (x: String) def combine(y: String): String = s"($x $y)"
    given Variable[String, String] with
        type Label = String
        def label(v: String): String    = v
        def describe(v: String): String = v
end Variable
