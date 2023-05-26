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
    given scalaObjects: ScalaObjects[FreeU] with
        given scalaOb[A]: Ob[FreeU[Scala[A]]] = FreeObj.Scala()
    given monoidalObjects: MonoidalObjects[FreeU] with
        given unitOb: Ob[FreeU[One]]                              = FreeObj.One
        given tensorOb[A, B](using a: Ob[A], b: Ob[B]): Ob[A x B] =
            a match
                case ao: FreeObj[A] =>
                    b match
                        case bo: FreeObj[B] =>
                            FreeObj.Prod(ao, bo)
end FreeU
