package volga.syntax.parsing

import volga.syntax.solve.PMagma

trait Labeled[V]:
    type Label
    def label(v: V): Label

    def toMap(vs: Vector[V]): Map[Label, V] =
        vs.map(v => label(v) -> v).toMap

trait Variable[V, Description](using val describeMagma: PMagma[Description]) extends Labeled[V]:
    def describe(v: V): Description
