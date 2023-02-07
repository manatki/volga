package volga.functors


trait Monoidal[F[_]] extends Functor[F]:
    def pure[A](a: A): F[A]
    extension [A](fa: F[A])
        def map2[B, C](fb: => F[B])(f: (A, B) => C): F[C]
        def >>[B](fb: => F[B]): F[B] = map2(fb)((_, b) => b)

        override def map[B](f: A => B): F[B] = fa.map2(pure(()))((a, _) => f(a))

object Monoidal:
    trait Cov[F[+_]] extends Functor.Cov[F], Monoidal[F]