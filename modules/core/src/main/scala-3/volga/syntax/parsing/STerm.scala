package volga.syntax.parsing

package Pos:
    trait Mid
    trait End
    trait Tupling

enum STerm[+S, +T]:
    case Assignment[+S, +T](receivers: Vector[S], application: Application[S, T]) extends STerm[S, T], Pos.Mid
    case Tupled[+S, +T](receiver: S, arity: Int, application: Application[S, T])  extends STerm[S, T], Pos.Tupling
    case Untupling[+S](src: S, tgt: S, index: Int)                                extends STerm[S, Nothing], Pos.Tupling
    case Result[+S](results: Vector[S])                                           extends STerm[S, Nothing], Pos.End
    case Application[+S, +T](applied: T, args: Vector[S])                         extends STerm[S, T], Pos.Mid, Pos.End
