package volga.syntax.internal

package Pos:
    final abstract class Mid
    final abstract class End

enum STerm[-Pos, +S, +T]:
    case Assignment[+S, +T](receivers: Vector[S], application: Application[S, T]) extends STerm[Pos.Mid, S, T]
    case Result[+S](results: Vector[S])                                           extends STerm[Pos.End, S, Nothing]
    case Application[+S, +T](applied: T, args: Vector[S])                         extends STerm[Any, S, T]


object STerm:

end STerm
