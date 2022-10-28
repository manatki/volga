package volga
package syntax

import volga.comp.{ArrVars, MkArr, MkSyMon, SMCVars}
import volga.impl.SyntaxMacro

object comp {
  class V[X]

  private object varro extends V[Any]
  private[volga] def varr[X]: V[X] = varro.asInstanceOf[V[X]]

  object ----

  def arr[P[_, _]] = new MkArr[P]

  def symon[↦[_, _], ⊗[_, _], I] = new MkSyMon[↦, ⊗, I]

  def ident[P[_, _], A](implicit arr: Identity[P]): P[A, A]                              = arr.id
  def liftf[P[_, _], A, B](f: A => B)(implicit arr: Arr[P]): P[A, B]                     = arr.lift(f)
  def liftf2[P[_, _], A1, A2, B](f: (A1, A2) => B)(implicit arr: Arr[P]): P[(A1, A2), B] = arr.lift(f.tupled)

  def smc[P[_, _], x[_, _], I](implicit mc: Symon[P, x, I]): mc.type = mc

  implicit class ArrSyn[P[_, _], A, B](val s: P[A, B])(implicit like: ArrLike[P]) {
    def apply[VB]()(implicit vb: ArrVars[B, VB]): VB                                                 = vb.va
    def apply[X1, VB](v: V[X1])(implicit vb: ArrVars[B, VB], ev: X1 <:< A): VB                       = vb.va
    def apply[X1, X2, VB](v1: V[X1], v2: V[X2])(implicit vb: ArrVars[B, VB], ev: (X1, X2) <:< A): VB = vb.va
    def apply[X1, X2, X3, VB](v1: V[X1], v2: V[X2], v3: V[X3])(
        implicit vb: ArrVars[B, VB],
        ev: (X1, X2, X3) <:< A
    ): VB = vb.va
    def apply[X1, X2, X3, X4, VB](v1: V[X1], v2: V[X2], v3: V[X3], v4: V[X4])(
        implicit vb: ArrVars[B, VB],
        ev: (X1, X2, X3, X4) <:< A
    ): VB = vb.va
  }

  implicit class SMCSyn[P[_, _], A, B, x[_, _], I](val s: P[A, B])(implicit like: MonCatLike[P, x, I]) {
    def apply[VB]()(implicit vb: SMCVars[B, VB, x, I]): VB                                                  = vb.va
    def apply[X1, VB](v: V[X1])(implicit vb: SMCVars[B, VB, x, I], ev: X1 <:< A): VB                        = vb.va
    def apply[X1, X2, VB](v1: V[X1], v2: V[X2])(implicit vb: SMCVars[B, VB, x, I], ev: (X1 x X2) <:< A): VB = vb.va
    def apply[X1, X2, X3, VB](v1: V[X1], v2: V[X2], v3: V[X3])(
        implicit vb: SMCVars[B, VB, x, I],
        ev: (X1 x X2 x X3) <:< A
    ): VB = vb.va
    def apply[X1, X2, X3, X4, VB](v1: V[X1], v2: V[X2], v3: V[X3], v4: V[X4])(
        implicit vb: SMCVars[B, VB, x, I],
        ev: (X1 x X2 x X3 x X4) <:< A
    ): VB = vb.va
  }
}
