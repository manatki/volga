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
import scala.collection.immutable.ArraySeq

case class Probs[B](
    values: Vector[B],
    probs: Array[Double]
)

case class CalculatedProbs[A, B](vs: Map[A, Probs[B]], mark: Mark[A, B]) extends (A => Probs[B]):
    def apply(v1: A): Probs[B] = vs.getOrElse(v1, mark.calculate(v1))

enum MarkQ[A, B]:
    case Range(n: Int)                      extends MarkQ[I, $[Int]]
    case Calculated[A, B](f: A => Probs[B]) extends MarkQ[$[A], $[B]]

object MarkQ extends ObAliases[FreeU]:
    type ProbData[A] = A match
        case tags.Scala[x]     => Probs[x]
        case tags.One          => Unit
        case tags.Tensor[a, b] => (ProbData[a], ProbData[b])

    extension [A, B](m: Mark[A, B]) def calculate(input: A): Probs[B] = ???

    private def calculate[A, B](m: Mark[A, B], inputs: ProbData[A]): ProbData[B]      =
        m match
            case FreeCat.Embed(q)                                 => quiverCalc(q, inputs)
            case FreeCat.Sequential(f, g)                         => calculate(g, calculate(f, inputs))
            case FreeCat.Ident()                                  => inputs
            case p: FreeCat.Parallel[FreeU, MarkBase, a, b, c, d] =>
                val splitA: (a x c) =:= A = <:<.refl
                ???
    private def quiverCalc[A, B](q: MarkBase[A, B], inputs: ProbData[A]): ProbData[B] = ???
end MarkQ

type MarkBase[A, B] = MarkQ[A, B] | Scalian[FreeU, A, B]

type Mark[A, B] = FreeCat[FreeU, MarkBase, A, B]

object Mark extends Aliases[Mark, FreeU]:
    summon[ApplyCat[Mark, FreeU]]
