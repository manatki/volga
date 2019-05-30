package volga

import cats.arrow.Arrow

object syntax {
  class V[X]

  private object varro extends V[Any]
  private def varr[X]: V[X] = varro.asInstanceOf[V[X]]

  object ----

  def arr[P[_, _]] = new MkArr[P]

  def symon[↦[_, _], ⊗[_, _], I] = new MkSyMon[↦, ⊗, I]

  def ident[P[_, _], A](implicit arr: Arr[P]): P[A, A] = arr.id
  def liftf[P[_, _], A, B](f: A => B)(implicit arr: Arr[P]): P[A, B] = arr.lift(f)
  def liftf2[P[_, _], A1, A2, B](f: (A1, A2) => B)(implicit arr: Arr[P]): P[(A1, A2), B] = arr.lift(f.tupled)

  class MkArr[P[_, _]] {
    def apply[VB, B](body: () => VB)(implicit vb: Vars[B, VB]): P[Unit, B] = macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A, VB, B](body: V[A] => VB)(implicit vb: Vars[B, VB]): P[A, B] = macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A1, A2, VB, B](body: (V[A1], V[A2]) => VB)(implicit vb: Vars[B, VB]): P[(A1, A2), B] =
    macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A1, A2, A3, VB, B](body: (V[A1], V[A2], V[A3]) => VB)(implicit vb: Vars[B, VB]): P[(A1, A2, A3), B] =
    macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A1, A2, A3, A4, VB, B](body: (V[A1], V[A2], V[A3], V[A4]) => VB)(
      implicit vb: Vars[B, VB]): P[(A1, A2, A3, A4), B] =
    macro SyntaxMacro.arr[P[Unit, Unit], B]
  }

  class MkSyMon[↦[_, _], ⊗[_, _], I] {
    def apply[VB, B](body: () => VB)(implicit vb: Vars[B, VB]): I ↦ B = macro SyntaxMacro.symmon[Unit ↦ Unit, ⊗[_, _], I,  B]

    def apply[A, VB, B](body: V[A] => VB)(implicit vb: Vars[B, VB]): A ↦ B = macro SyntaxMacro.symmon[Unit ↦ Unit, ⊗[_, _], I,  B]

    def apply[A1, A2, VB, B](body: (V[A1], V[A2]) => VB)(implicit vb: Vars[B, VB]): (A1 ⊗ A2) ↦ B =
    macro SyntaxMacro.symmon[Unit ↦ Unit, ⊗[_, _], I,  B]

    def apply[A1, A2, A3, VB, B](body: (V[A1], V[A2], V[A3]) => VB)(implicit vb: Vars[B, VB]): (A1 ⊗ A2 ⊗ A3) ↦ B =
    macro SyntaxMacro.symmon[Unit ↦ Unit, ⊗[_, _], I,  B]

    def apply[A1, A2, A3, A4, VB, B](body: (V[A1], V[A2], V[A3], V[A4]) => VB)(
      implicit vb: Vars[B, VB]): (A1 ⊗ A2 ⊗ A3 ⊗ A4) ↦ B = macro SyntaxMacro.symmon[Unit ↦ Unit, ⊗[_, _], I,  B]
  }

  implicit class ArrSyn[P[_, _], A, B](val s: P[A, B]) extends AnyVal {
    def apply[VB]()(implicit vb: Vars[B, VB]): VB = vb.va
    def apply[X1, VB](v: V[X1])(implicit vb: Vars[B, VB], ev: X1 <:< A): VB = vb.va
    def apply[X1, X2, VB](v1: V[X1], v2: V[X2])(implicit vb: Vars[B, VB], ev: (X1, X2) <:< A): VB = vb.va
    def apply[X1, X2, X3, VB](v1: V[X1], v2: V[X2], v3: V[X3])(implicit vb: Vars[B, VB], ev: (X1, X2, X3) <:< A): VB = vb.va
    def apply[X1, X2, X3, X4, VB](v1: V[X1], v2: V[X2], v3: V[X3], v4: V[X4])(implicit vb: Vars[B, VB],
                                                                              ev: (X1, X2, X3, X4) <:< A): VB = vb.va
  }

  final case class Vars[A, VA] private(va: VA)

  object Vars extends LowLevelVars {
    implicit val varIn0: Vars[Unit, V[Unit]] = Vars(varr)

    implicit def varIn2[A, B]: Vars[(A, B), (V[A], V[B])] = Vars((varr, varr))
    implicit def varIn3[A, B, C]: Vars[(A, B, C), (V[A], V[B], V[C])] = Vars((varr, varr, varr))
    implicit def varIn4[A, B, C, D]: Vars[(A, B, C, D), (V[A], V[B], V[C], V[D])] = Vars((varr, varr, varr, varr))
    implicit def varIn5[A, B, C, D, E]: Vars[(A, B, C, D, E), (V[A], V[B], V[C], V[D], V[E])] =
      Vars((varr, varr, varr, varr, varr))
  }

  trait LowLevelVars {
    implicit def varIn1[A]: Vars[A, V[A]] = Vars(varr)
  }
}
