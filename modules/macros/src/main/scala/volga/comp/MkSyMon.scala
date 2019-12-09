package volga.comp
import volga.impl.SyntaxMacro
import volga.syntax.comp.V

class MkSyMon[↦[_, _], x[_, _], I] {
  def apply[VB, B](body: () => VB)(implicit vb: SMCVars[B, VB, x, I]): I ↦ B =
    macro SyntaxMacro.symmon[Unit ↦ Unit, x[_, _], I, B]

  def apply[A, VB, B](body: V[A] => VB)(implicit vb: SMCVars[B, VB, x, I]): A ↦ B =
    macro SyntaxMacro.symmon[Unit ↦ Unit, x[_, _], I, B]

  def apply[A1, A2, VB, B](body: (V[A1], V[A2]) => VB)(implicit vb: SMCVars[B, VB, x, I]): (A1 x A2) ↦ B =
    macro SyntaxMacro.symmon[Unit ↦ Unit, x[_, _], I, B]

  def apply[A1, A2, A3, VB, B](
      body: (V[A1], V[A2], V[A3]) => VB
  )(implicit vb: SMCVars[B, VB, x, I]): (A1 x A2 x A3) ↦ B =
    macro SyntaxMacro.symmon[Unit ↦ Unit, x[_, _], I, B]

  def apply[A1, A2, A3, A4, VB, B](body: (V[A1], V[A2], V[A3], V[A4]) => VB)(
      implicit vb: SMCVars[B, VB, x, I]
  ): (A1 x A2 x A3 x A4) ↦ B = macro SyntaxMacro.symmon[Unit ↦ Unit, x[_, _], I, B]
}
