package volga
package prob

import volga.tags.Scala
import volga.tags.Obj
import volga.tags.Tensor
import volga.tags.One

import volga.{Aliases}
import volga.tags.{Obj, One, Tensor, Scala}
import volga.free.{FreeObj, FreeU}
import volga.free.FreeSMC
import volga.free.FreeCat
import volga.free.FreeCat.Scalian

enum MarkQ[A, B]:
    case Range(n: Int) extends MarkQ[I, $[Int]]

object MarkQ extends ObAliases[FreeU]

type MarkBase[A, B] = MarkQ[A, B] | Scalian[FreeU, A, B]

type Mark[A, B] = FreeCat[FreeU, MarkBase, A, B]

object Mark extends Aliases[Mark, FreeU]:
    summon[ApplyCat[Mark, FreeU]]
