package volga.functors
package instances

import scala.annotation.tailrec

given idFunctor: TraverseMonad[[x] =>> x] with
    def pure[A](x: A): A = x

    override def rightAssoc: Boolean = false
    extension [A](a: A)
        override def map[B](f: A => B): B                = f(a)
        override def map2[B, C](b: => B)(f: (A, B) => C) = f(a, b)

        def flatMap[B](f: A => B)                      = f(a)
        def traverse[M[+_]: Monoidal, B](f: A => M[B]) = f(a)

    @tailrec override def recursion[A, B](a: A)(f: A => Either[A, B]): B =
        f(a) match
            case Left(a)  => recursion(a)(f)
            case Right(b) => b
end idFunctor
