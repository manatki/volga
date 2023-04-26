package volga.functors.instances

import volga.functors.Monad
import volga.functors.Traverse
import volga.functors.Monoidal

given eitherMonad[L]: Monad[[x] =>> Either[L, x]] with Traverse[[x] =>> Either[L, x]] with
    def pure[A](a: A) = Right(a)

    extension [A](fa: Either[L, A])
        def flatMap[B](f: A => Either[L, B]) = fa.flatMap(f)
        def traverse[G[+_], B](f: A => G[B])(using G: Monoidal[G]): G[Either[L, B]] = fa match
            case Right(a) => f(a).map(Right(_))
            case Left(l)  => G.pure(Left(l))
end eitherMonad
