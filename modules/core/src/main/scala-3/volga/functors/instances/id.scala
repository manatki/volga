package volga.functors
package instances

given idFunctor: TraverseMonad[[x] =>> x] with
    def pure[A](x: A): A = x
    extension [A](a: A)
        override def map[B](f: A => B): B                = f(a)
        override def map2[B, C](b: => B)(f: (A, B) => C) = f(a, b)

        def flatMap[B](f: A => B)                     = f(a)
        def traverse[M[+_]: Monoidal, B](f: A => M[B]) = f(a)
