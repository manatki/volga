package volga
package prob

import volga.tags.Scala
import volga.tags.Obj
import volga.tags.Tensor
import volga.tags.One

enum MarkQ[A, B]:
  case Range(n: Int)                      extends (I --> $[Int])
  case MergeWith[A, B, C](f: (A, B) => C) extends (($[A] x $[B]) --> $[C])

object MarkQ extends Aliases[MarkQ, FreeU]:
  given HasScalaFunctor[Mark, FreeU] with
    def lift[A, B](f: A => B): $[A] --> $[B] = ???


type Mark[A, B] = Mark.Free[A, B]

object Mark extends FreeSMC[MarkQ, FreeU] with Aliases[Mark, FreeU]:

  def unitObj: Ob[I] = FreeU.Obj

  def tensorObj[A: Ob, B: Ob]: Ob[A x B] = FreeU.Obj

  def range(x: Int): I --> $[Int] = embed[I, $[Int]](MarkQ.Range(x))
  def unit: I --> $[Unit] = ???
