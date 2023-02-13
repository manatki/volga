package volga.syntax.parsing

import volga.functors.*
import volga.syntax.solve.Permutations
import volga.syntax.solve.Bin
import volga.syntax.solve.BinOp
import volga.syntax.solve.Adaptation
import volga.syntax.solve.BinRes
import volga.syntax.solve.PMagma
import scala.annotation.tailrec



case class App[+S, +T](applied: T, args: Vector[S]) derives Traverse

package Pos:
    trait Mid
    trait End
    trait Tupling

enum STerm[+S, +T] derives Traverse:
    case Assignment[+S, +T](receivers: Vector[S], application: App[S, T]) extends STerm[S, T], Pos.Mid
    case Tupled[+S, +T](receiver: S, arity: Int, application: App[S, T])  extends STerm[S, T], Pos.Tupling
    case Untupling[+S](src: S, tgt: S, index: Int)                        extends STerm[S, Nothing], Pos.Tupling
    case Result[+S](results: Vector[S])                                   extends STerm[S, Nothing], Pos.End
    case Application[+S, +T](applied: App[S, T])                          extends STerm[S, T], Pos.Mid, Pos.End
