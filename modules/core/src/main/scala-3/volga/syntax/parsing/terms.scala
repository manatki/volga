package volga.syntax.parsing
import volga.functors.*

object VectorOf:
    opaque type VectorOf[+F[+_], +A] <: Vector[F[A]] = Vector[F[A]]

    def apply[F[+_], A](fa: Vector[F[A]]): VectorOf[F, A] = fa

    given [F[+_]: Functor]: Functor[VectorOf[F, _]] = summon[Functor[Vector]].composeFunctor[F]

    given [F[+_]: Traverse]: Traverse[VectorOf[F, _]] = summon[Traverse[Vector]].composeTraverse[F]

type VectorOf[+F[+_], +A] = VectorOf.VectorOf[F, A]

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

case class Stage[+T, +O](term: T, outputs: O) derives Traverse

case class StageList[+T, +O](stages: VectorOf[Stage[T, _], O], result: O) derives Traverse
