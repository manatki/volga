package volga.functors

import scala.annotation.threadUnsafe
import scala.collection.mutable.Builder
import scala.collection.Factory
import scala.annotation.tailrec
import volga.functors.Monad.Recur

trait Monad[F[+_]] extends Monoidal[F]:
    extension [A](fa: F[A])
        def flatMap[B](f: A => F[B]): F[B]
        override def map2[B, C](fb: => F[B])(f: (A, B) => C): F[C] =
            for a <- fa; b <- fb yield f(a, b)
        override def >>[B](fb: => F[B])                            = fa.flatMap(_ => fb)
        override def map[B](f: A => B)                             = fa.flatMap(a => pure(f(a)))

    end extension
    def recursion[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
        f(a).flatMap {
            case Left(a)  => recursion(a)(f)
            case Right(b) => pure(b)
        }

    def recur[A, B](a: A)(f: A => Recur[F, A, B]): F[B] =
        @tailrec def go(r: Recur[F, A, B]): F[B] = r match
            case Recur.Yield(a1) => go(f(a1))
            case Recur.Done(b)   => pure(b)
            case Recur.Lift(fr)  => fr.flatMap(go1)

        def go1(r: Recur[F, A, B]) = go(r)
        go(f(a))

end Monad

object Monad:
    def collectionRecursion[F[+a] <: Iterable[a], A, B](
        fact: Factory[B, F[B]],
        first: A,
        f: A => F[Either[A, B]]
    ): F[B] =
        val builder = fact.newBuilder

        @tailrec def go(stack: List[Either[A, B]]): Unit = stack match
            case Left(a) :: tail   => go(f(a) ++: tail)
            case Right(fb) :: tail =>
                builder += fb
                go(tail)
            case Nil               =>

        go(List(Left(first)))
        builder.result()
    end collectionRecursion

    def collectionRecur[F[+a] <: Iterable[a], A, B](
        fact: Factory[B, F[B]],
        first: A,
        f: A => Recur[F, A, B]
    ): F[B] =
        val builder = fact.newBuilder

        @tailrec def go(stack: List[Recur[F, A, B]]): Unit = stack match
            case Recur.Yield(a) :: tail => go(f(a) :: tail)
            case Recur.Done(b) :: tail  =>
                builder += b
                go(tail)
            case Recur.Lift(fs) :: tail => go(fs ++: tail)
            case Nil                    =>

        go(List(Recur.Yield(first)))
        builder.result()
    end collectionRecur

    enum Recur[+F[+_], A, B]:
        case Yield(a: A)
        case Done(b: B)
        case Lift(fr: F[Recur[F, A, B]])

end Monad
