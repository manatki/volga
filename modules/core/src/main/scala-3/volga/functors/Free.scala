package volga.functors

enum Free[+F[+_], +A]:
    case Pure(a: A)
    case Embed(fa: F[A])
    case Bind[+F[+_], A, +B](fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

    def flatMap[F1[+x] >: F[x], B](f: A => Free[F1, B]): Free[F1, B] = Bind(this, f)

    final def foldMap[M[+x]](t: [A] => F[A] => M[A])(using M: Monad[M]): M[A] =
        M.recursion(this) {
            case Pure(a)     => M.pure(Right(M.pure(a)))
            case Embed(fa)   => t(fa).map(a => Left(Free.Pure(a)))
            case Bind(fa, f) =>
                fa match
                    case Pure(a)     => M.pure(Left(f(a)))
                    case Embed(fa)   => t(fa).map(x => Left(f(x)))
                    case Bind(fb, g) => M.pure(Left(fb.flatMap(x => g(x).flatMap(f))))
        }

    end foldMap
end Free

object Free:
    val unit: Free[Nothing, Unit] = Pure(())

    given [F[+_]]: Monad[Free[F, _]] with
        def pure[A](a: A)                                                = Pure(a)
        extension [A](fa: Free[F, A]) def flatMap[B](f: A => Free[F, B]) = fa.flatMap(f)
