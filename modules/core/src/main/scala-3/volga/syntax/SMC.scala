package volga
package syntax

import volga.SymmetricCat

import volga.{Aliases, SymmetricCat}
object smc {
  abstract final class Var[T]

  final class Syntax[H[_, _], U[_]](using c: SymmetricCat[H, U]) extends Aliases[H, U]:
    final abstract class SyApp:
      extension [A, B](f: H[A, B]) def apply(v: Var[A]): Var[B]

    type Reconstruct[X] = X match
      case Var[a]     => a
      case EmptyTuple => I
      case h *: t     => Reconstruct[h] x Reconstruct[t]

    inline def apply[R](inline f: () => SyApp ?=> R): H[R, Reconstruct[R]] = null.asInstanceOf[Nothing]
    inline def apply[A, B](inline f: Var[A] => SyApp ?=> B): H[A, Reconstruct[B]] = null.asInstanceOf[Nothing]

  end Syntax

  inline def syntax[H[_, _], U[_]](using SymmetricCat[H, U]): Syntax[H, U] = new Syntax
}
