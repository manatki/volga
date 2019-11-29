package volga
package syntax

import volga.impl.SyntaxMacro

object comp {
  class V[X]

  private object varro extends V[Any]
  private def varr[X]: V[X] = varro.asInstanceOf[V[X]]

  object ----

  def arr[P[_, _]] = new MkArr[P]

  def symon[↦[_, _], ⊗[_, _], I] = new MkSyMon[↦, ⊗, I]

  def ident[P[_, _], A](implicit arr: Identity[P]): P[A, A]                              = arr.id
  def liftf[P[_, _], A, B](f: A => B)(implicit arr: Arr[P]): P[A, B]                     = arr.lift(f)
  def liftf2[P[_, _], A1, A2, B](f: (A1, A2) => B)(implicit arr: Arr[P]): P[(A1, A2), B] = arr.lift(f.tupled)

  class MkArr[P[_, _]] {
    def apply[VB, B](body: () => VB)(implicit vb: ArrVars[B, VB]): P[Unit, B] = macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A, VB, B](body: V[A] => VB)(implicit vb: ArrVars[B, VB]): P[A, B] =
      macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A1, A2, VB, B](body: (V[A1], V[A2]) => VB)(implicit vb: ArrVars[B, VB]): P[(A1, A2), B] =
      macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A1, A2, A3, VB, B](body: (V[A1], V[A2], V[A3]) => VB)(implicit vb: ArrVars[B, VB]): P[(A1, A2, A3), B] =
      macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A1, A2, A3, A4, VB, B](body: (V[A1], V[A2], V[A3], V[A4]) => VB)(
        implicit vb: ArrVars[B, VB]): P[(A1, A2, A3, A4), B] =
      macro SyntaxMacro.arr[P[Unit, Unit], B]
  }

  class MkSyMon[↦[_, _], ⊗[_, _], I] {
    def apply[VB, B](body: () => VB)(implicit vb: ArrVars[B, VB]): I ↦ B =
      macro SyntaxMacro.symmon[Unit ↦ Unit, ⊗[_, _], I, B]

    def apply[A, VB, B](body: V[A] => VB)(implicit vb: ArrVars[B, VB]): A ↦ B =
      macro SyntaxMacro.symmon[Unit ↦ Unit, ⊗[_, _], I, B]

    def apply[A1, A2, VB, B](body: (V[A1], V[A2]) => VB)(implicit vb: ArrVars[B, VB]): (A1 ⊗ A2) ↦ B =
      macro SyntaxMacro.symmon[Unit ↦ Unit, ⊗[_, _], I, B]

    def apply[A1, A2, A3, VB, B](body: (V[A1], V[A2], V[A3]) => VB)(implicit vb: ArrVars[B, VB]): (A1 ⊗ A2 ⊗ A3) ↦ B =
      macro SyntaxMacro.symmon[Unit ↦ Unit, ⊗[_, _], I, B]

    def apply[A1, A2, A3, A4, VB, B](body: (V[A1], V[A2], V[A3], V[A4]) => VB)(
        implicit vb: ArrVars[B, VB]): (A1 ⊗ A2 ⊗ A3 ⊗ A4) ↦ B = macro SyntaxMacro.symmon[Unit ↦ Unit, ⊗[_, _], I, B]
  }

  implicit class ArrSyn[P[_, _], A, B](val s: P[A, B])(implicit like: ArrLike[P]) {
    def apply[VB]()(implicit vb: ArrVars[B, VB]): VB                                                 = vb.va
    def apply[X1, VB](v: V[X1])(implicit vb: ArrVars[B, VB], ev: X1 <:< A): VB                       = vb.va
    def apply[X1, X2, VB](v1: V[X1], v2: V[X2])(implicit vb: ArrVars[B, VB], ev: (X1, X2) <:< A): VB = vb.va
    def apply[X1, X2, X3, VB](v1: V[X1], v2: V[X2], v3: V[X3])(implicit vb: ArrVars[B, VB],
                                                               ev: (X1, X2, X3) <:< A): VB = vb.va
    def apply[X1, X2, X3, X4, VB](v1: V[X1], v2: V[X2], v3: V[X3], v4: V[X4])(implicit vb: ArrVars[B, VB],
                                                                              ev: (X1, X2, X3, X4) <:< A): VB = vb.va
  }

  implicit class SMCSyn[P[_, _], A, B, x[_, _], I](val s: P[A, B])(implicit like: MonCatLike[P, x, I]) {
    def apply[VB]()(implicit vb: SMCVars[B, VB, x, I]): VB                                                  = vb.va
    def apply[X1, VB](v: V[X1])(implicit vb: SMCVars[B, VB, x, I], ev: X1 <:< A): VB                        = vb.va
    def apply[X1, X2, VB](v1: V[X1], v2: V[X2])(implicit vb: SMCVars[B, VB, x, I], ev: (X1 x X2) <:< A): VB = vb.va
    def apply[X1, X2, X3, VB](v1: V[X1], v2: V[X2], v3: V[X3])(implicit vb: SMCVars[B, VB, x, I],
                                                               ev: (X1 x X2 x X3) <:< A): VB = vb.va
    def apply[X1, X2, X3, X4, VB](v1: V[X1], v2: V[X2], v3: V[X3], v4: V[X4])(implicit vb: SMCVars[B, VB, x, I],
                                                                              ev: (X1 x X2 x X3 x X4) <:< A): VB = vb.va
  }

  final case class ArrVars[A, VA] private (va: VA)

  object ArrVars extends LowLevelArrVars {
    implicit val varIn0: ArrVars[Unit, V[Unit]] = ArrVars(varr)

    implicit def varIn2[A, B]: ArrVars[(A, B), (V[A], V[B])]                         = ArrVars((varr, varr))
    implicit def varIn3[A, B, C]: ArrVars[(A, B, C), (V[A], V[B], V[C])]             = ArrVars((varr, varr, varr))
    implicit def varIn4[A, B, C, D]: ArrVars[(A, B, C, D), (V[A], V[B], V[C], V[D])] = ArrVars((varr, varr, varr, varr))
    implicit def varIn5[A, B, C, D, E]: ArrVars[(A, B, C, D, E), (V[A], V[B], V[C], V[D], V[E])] =
      ArrVars((varr, varr, varr, varr, varr))
  }

  trait LowLevelArrVars {
    implicit def varIn1[A]: ArrVars[A, V[A]] = ArrVars(varr)
  }

  final case class SMCVars[A, VA, x[_, _], I] private (va: VA)

  object SMCVars extends LowLevelSMCVars {

    implicit def varIn0[x[_, _], I]: SMCVars[I, V[I], x, I] = SMCVars(varr)

    implicit def varIn2[x[_, _], I, A, B]: SMCVars[(A x B), (V[A], V[B]), x, I] = SMCVars((varr, varr))
    implicit def varIn3[x[_, _], I, A, B, C]: SMCVars[(A x B x C), (V[A], V[B], V[C]), x, I] =
      SMCVars((varr, varr, varr))
    implicit def varIn4[x[_, _], I, A, B, C, D]: SMCVars[(A x B x C x D), (V[A], V[B], V[C], V[D]), x, I] =
      SMCVars((varr, varr, varr, varr))
    implicit def varIn5[x[_, _], I, A, B, C, D, E]: SMCVars[(A x B x C x D x E), (V[A], V[B], V[C], V[D], V[E]), x, I] =
      SMCVars((varr, varr, varr, varr, varr))
  }

  trait LowLevelSMCVars {
    implicit def varIn1[x[_, _], I, A]: SMCVars[A, V[A], x, I] = SMCVars(varr)
  }

}
