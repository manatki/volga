package volga
package free

import volga.ObAliases
import volga.tags.{Obj, One, Tensor, Scala}
sealed trait FreeU[X]

enum FreeObj[X] extends FreeU[tags.Obj[X]]:
  case One                                      extends FreeObj[FreeU[tags.One]]
  case Scala[A]()                               extends FreeObj[FreeU[tags.Scala[A]]]
  case Prod[A, B](l: FreeObj[A], r: FreeObj[B]) extends FreeObj[FreeU[tags.Tensor[A, B]]]

object FreeU extends ObAliases[FreeU]:
  given forScala[A]: Ob[$[A]]                                = FreeObj.Scala()
  given forOne: Ob[I]                                        = FreeObj.One
  given forTensor[A, B](using a: Ob[A], b: Ob[B]): Ob[A x B] =
    a match
      case ao: FreeObj[A] =>
        b match
          case bo: FreeObj[B] =>
            FreeObj.Prod(ao, bo)
