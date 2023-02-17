package volga.syntax.parsing

import volga.syntax.solve.PMagma

trait Labeled[V]:
    type Label
    def label(v: V): Label

trait Variable[V, Description](using val describeMagma: PMagma[Description]) extends Labeled[V]:
    def describe(v: V): Description
