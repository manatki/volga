package volga.functors.instances

import volga.functors.Monad


given functionMonad[R]: Monad[[x] =>> R => x] with 
    def pure[A](a: A) = _ => a

    extension [A](fa: R => A)
        def flatMap[B](f: A => R => B) = r => f(fa(r))(r)

end functionMonad