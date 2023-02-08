package volga.functors

trait Monad[F[+_]] extends Monoidal[F]:
    extension [A](fa: F[A])
        def flatMap[B](f: A => F[B]): F[B]
        override def map2[B, C](fb: => F[B])(f: (A, B) => C): F[C] =
            for a <- fa; b <- fb yield f(a, b)
        override def >>[B](fb: => F[B])                            = fa.flatMap(_ => fb)
        override def map[B](f: A => B)                             = fa.flatMap(a => pure(f(a)))
