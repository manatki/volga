package volga.free

import annotation.tailrec
import compiletime.ops.int.{<=, -}
import compiletime.{constValue, error, erasedValue}

enum Nat[n]:
    case Zero()             extends Nat[Zero]
    case Succ[n](p: Nat[n]) extends Nat[Succ[n]]

object Nat:
    def apply[n](using n: Nat[n]): Nat[n] = n

    given Nat[Zero] = Zero()

    def teq[A, B](using eq: A =:= B): A =:= B = eq

    given [n](using n: Nat[n]): Nat[Succ[n]] = Succ(n)

    type Plus[x, y] = y match
        case Zero     => x
        case Succ[y1] => Succ[Plus[x, y1]]

    type `0` = Zero
    type `1` = Succ[Zero]
    type `2` = Succ[Succ[Zero]]

    def plus[n: Nat, m: Nat]: Nat[Plus[n, m]] = Nat[m] match
        case Zero()              => Nat[n]
        case Succ(given Nat[m1]) =>
            teq[Succ[m1], m].liftCo[Plus[n, _]].substituteCo(Succ(plus[n, m1]))

    type OfInt[n <: Int] <: Nat[?] = (n <= 0) match
        case true  => Zero
        case false => Succ[OfInt[n - 1]]

    infix type +[A, B] = Plus[A, B]

    def rzero[n]: Plus[n, Zero] =:= n = <:<.refl

    inline def erase[A, B](inline proof: => A =:= B): A =:= B = <:<.refl.asInstanceOf[A =:= B]

    def lzero[n: Nat]: Plus[Zero, n] =:= n =
        Nat[n] match
            case Zero()              => <:<.refl
            case Succ(given Nat[n1]) =>
                teq[n, Succ[n1]].liftCo[[x] =>> Plus[Zero, x]] andThen lzero[n1].liftCo[Succ]

    def assoc[a: Nat, b: Nat, c: Nat]: Plus[Plus[a, b], c] =:= Plus[a, Plus[b, c]] =
        Nat[c] match
            case Zero()              => <:<.refl
            case Succ(given Nat[c1]) =>
                teq[c, Succ[c1]].liftCo[Plus[Plus[a, b], _]] andThen
                    assoc[a, b, c1].liftCo[Succ] andThen
                    teq[Succ[c1], c].liftCo[[x] =>> Plus[a, Plus[b, x]]]
             
    def succPlus[a, b: Nat]: Succ[Plus[a, b]] =:= Plus[Succ[a], b] =
        Nat[b] match
            case Zero()              => <:<.refl
            case Succ(given Nat[b1]) =>
                teq[b, Succ[b1]].liftCo[[x] =>> Succ[Plus[a, x]]] andThen
                    succPlus[a, b1].liftCo[Succ] andThen
                    teq[Succ[b1], b].liftCo[[x] =>> Plus[Succ[a], x]]

    def comm[a: Nat, b: Nat]: Plus[a, b] =:= Plus[b, a] =
        Nat[b] match
            case Zero()              => lzero[a].flip
            case Succ(given Nat[b1]) =>
                teq[b, Succ[b1]].liftCo[[x] =>> Plus[a, x]] andThen
                    comm[a, b1].liftCo[Succ] andThen
                    succPlus[b1, a] andThen
                    teq[Succ[b1], b].liftCo[[x] =>> Plus[x, a]]

    type Vec[N, +A] <: Tuple = N match
        case Nat.Zero    => EmptyTuple
        case Nat.Succ[n] => A *: Vec[n, A]

    extension [N: Nat, A](v: Vec[N, A])
        def toVector: Vector[A]                                                 = Nat[N] match
            case Zero()             => Vector.empty
            case Succ(given Nat[n]) =>
                teq[N, Succ[n]].liftCo[Vec[_, A]](v) match
                    case h *: t => h +: t.toVector

        @tailrec def reverseConcat[M, R, B](acc: Vec[M, B], eq: Plus[M, N] =:= R): Vec[R, A | B] =
            Nat[N] match
                case Zero()             => eq.liftCo[Vec[_, B]](acc)
                case Succ(given Nat[n]) =>
                    teq[N, Succ[n]].liftCo[Vec[_, A]].apply(v) match
                        case h *: t =>
                            t.reverseConcat[Succ[M], R, A | B](
                              h *: acc,
                              succPlus[M, n].flip andThen teq[Succ[n], N].liftCo[Plus[M, _]] andThen eq
                            )

        def reverse: Vec[N, A] = reverseConcat[Zero, N, A](EmptyTuple, lzero)

        def concat[M: Nat, B](v1: Vec[M, B]): Vec[Plus[N, M], A | B] =
            v.reverse.reverseConcat(v1, comm[M, N])

    end extension

    object Vec:
        def replicate[N: Nat, A](a: A): Vec[N, A] = Nat[N] match
            case Zero()             => EmptyTuple
            case Succ(given Nat[n]) => teq[Succ[n], N].liftCo[Vec[_, A]](a *: replicate(a))

        def tabulate[N: Nat, A](f: Int => A, start: Int = 0): Vec[N, A] = Nat[N] match
            case Zero()             => EmptyTuple
            case Succ(given Nat[n]) => teq[Succ[n], N].liftCo[Vec[_, A]](f(start) *: tabulate(f, start + 1))

        @tailrec private def split1[N: Nat, M, K, R: Nat, A](
            v: Vec[Plus[M, N], A],
            acc: Vec[K, A],
            eq: Plus[K, N] =:= R
        ): (Vec[M, A], Vec[R, A]) =
            Nat[N] match
                case Zero()             => (v, eq.liftCo[Vec[_, A]](acc).reverse)
                case Succ(given Nat[n]) =>
                    teq[N, Succ[n]].liftCo[[x] =>> Vec[Plus[M, x], A]](v) match
                        case h *: t =>
                            val eq1: Plus[Succ[K], n] =:= R =
                                succPlus[K, n].flip andThen teq[Succ[n], N].liftCo[Plus[K, _]] andThen eq
                            split1[n, M, Succ[K], R, A](t, h *: acc, eq1)

        def split[N: Nat, M: Nat, A](v: Vec[Plus[N, M], A]): (Vec[N, A], Vec[M, A]) =
            split1[N, M, Zero, N, A](erase(comm[N, M].liftCo[Vec[_, A]]).apply(v), EmptyTuple, lzero).swap

    end Vec

end Nat


trait Foo[X, Y]:
    type T >: X <: Y

import scala.compiletime.ops.int.*

abstract class NatLTE[A <: Int, B <: Int]:
    type C <: Int
    def ev: (A + C) =:= B

def ltAsGt[A <: Int, B <: Int](using ev: (A < B) =:= true): (B > A) =:= true = 
    ev.asInstanceOf[(B > A) =:= true]

def bar(foo: Foo[Int, String]): (Int <:< String) = 
    val ev: foo.T <:< String = <:<.refl
    ev
