package volga.syntax.internal

package Pos:
    final abstract class Mid
    final abstract class End
    final abstract class Tupling

enum STerm[-Pos, +S, +T]:
    case Assignment[+S, +T](receivers: Vector[S], application: Application[S, T]) extends STerm[Pos.Mid, S, T]
    case Tupled[+S, +T](receiver: S, arity: Int, application: Application[S, T])  extends STerm[Pos.Tupling, S, T]
    case Untupling[+S](src: S, tgt: S, index: Int)                                extends STerm[Pos.Tupling, S, Nothing]
    case Result[+S](results: Vector[S])                                           extends STerm[Pos.End, S, Nothing]
    case Application[+S, +T](applied: T, args: Vector[S])                         extends STerm[Any, S, T]
    case Dummy(tree: T)

object STerm:

end STerm
