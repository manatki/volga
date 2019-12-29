package volga.syntax

import volga.SemigropalCat

trait SymonOps {
  implicit class Ops[P[_,_], A, B](private val pab: P[A, B]) {
    def split[C, D, x[_, _]](pcd: P[C, D])(implicit sem: SemigropalCat[P, x]): P[A x C, B x D] = sem.split(pab, pcd)
  }
}
