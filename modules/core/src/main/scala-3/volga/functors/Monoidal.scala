package volga.functors

import scala.annotation.tailrec

trait Monoidal[F[+_]] extends Functor[F]:
    self =>
    def pure[A](a: A): F[A]
    extension [A](fa: F[A])
        def map2[B, C](fb: => F[B])(f: (A, B) => C): F[C]
        def >>[B](fb: => F[B]): F[B] = map2(fb)((_, b) => b)

        override def map[B](f: A => B): F[B] = fa.map2(pure(()))((a, _) => f(a))

    def rightAssoc: Boolean = true

    def tabulateTraverse[A, B](n: Int)(f: Int => F[B]): F[List[B]] =
        if rightAssoc then
            def go(i: Int): F[List[B]] =
                if i == n then pure(Nil)
                else f(i).map2(go(i + 1))(_ :: _)
            go(0)
        else
            def go(i: Int, acc: F[List[B]]): F[List[B]] =
                if i == -1 then acc
                else go(i - 1, f(i).map2(acc)(_ :: _))

            go(n - 1, pure(Nil))

    given composeMonoidal[G[+_]](using G: Monoidal[G]): Monoidal[[a] =>> F[G[a]]] with
        def pure[A](a: A): F[G[A]]                                   = self.pure(G.pure(a))
        extension [A](fga: F[G[A]])
            def map2[B, C](fgb: => F[G[B]])(f: (A, B) => C): F[G[C]] =
                self.map2(fga)(fgb)(_.map2(_)(f))
end Monoidal
