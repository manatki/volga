package volga
package syntax

object arr extends Arr.ToArrOps
object cat extends Cat.ToCatOps
object symmon {
  implicit class SymMonOps[P[_, _], A, B](private val pab: P[A, B]) extends AnyVal {
    def split[C, D, x[_, _]](pcd: P[C, D])(implicit sem: SemigropalCat[P, x]): P[A x C, B x D] = sem.split(pab, pcd)
  }
}
