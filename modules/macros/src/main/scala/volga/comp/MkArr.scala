package volga
package comp
import volga.impl.SyntaxMacro
import volga.syntax.comp.V

class MkArr[P[_, _]] {
    def apply[VB, B](body: () => VB)(implicit vb: ArrVars[B, VB]): P[Unit, B] = macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A, VB, B](body: V[A] => VB)(implicit vb: ArrVars[B, VB]): P[A, B] =
      macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A1, A2, VB, B](body: (V[A1], V[A2]) => VB)(implicit vb: ArrVars[B, VB]): P[(A1, A2), B] =
      macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A1, A2, A3, VB, B](body: (V[A1], V[A2], V[A3]) => VB)(implicit vb: ArrVars[B, VB]): P[(A1, A2, A3), B] =
      macro SyntaxMacro.arr[P[Unit, Unit], B]

    def apply[A1, A2, A3, A4, VB, B](
        body: (V[A1], V[A2], V[A3], V[A4]) => VB
    )(implicit vb: ArrVars[B, VB]): P[(A1, A2, A3, A4), B] =
      macro SyntaxMacro.arr[P[Unit, Unit], B]
  }