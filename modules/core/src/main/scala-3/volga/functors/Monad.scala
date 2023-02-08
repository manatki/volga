package volga.functors

import scala.annotation.threadUnsafe
import scala.collection.mutable.Builder
import scala.collection.Factory

trait Monad[F[+_]] extends Monoidal[F]:
    extension [A](fa: F[A])
        def flatMap[B](f: A => F[B]): F[B]
        override def map2[B, C](fb: => F[B])(f: (A, B) => C): F[C] =
            for a <- fa; b <- fb yield f(a, b)
        override def >>[B](fb: => F[B])                            = fa.flatMap(_ => fb)
        override def map[B](f: A => B)                             = fa.flatMap(a => pure(f(a)))

    end extension
    def recursion[A, B](a: A)(f: A => F[Either[A, F[B]]]): F[B] =
        f(a).flatMap {
            case Left(a)   => recursion(a)(f)
            case Right(fb) => fb
        }

end Monad

object Monad:
    def collectionRecursion[F[+a] <: Iterable[a], A, B](
        fact: Factory[B, F[B]],
        first: A,
        f: A => F[Either[A, F[B]]]
    ): F[B] =
        val builder                                = fact.newBuilder
        def go(stack: List[Either[A, F[B]]]): Unit = stack match
            case Left(a) :: tail   => go(f(a) ++: stack)
            case Right(fb) :: tail => builder ++= fb
            case Nil               =>

        builder.result()
    end collectionRecursion

end Monad
