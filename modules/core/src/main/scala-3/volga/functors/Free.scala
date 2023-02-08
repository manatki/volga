package volga.functors

enum Free[+F[+_], +A]:
    case Pure(a: A)
    case Embed(fa: F[A])
    case Bind[+F[+_], A, +B](fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

    def flatMap[F1[+x] >: F[x], B](f: A => Free[F1, B]): Free[F1, B] = Bind(this, f)

    final def foldMap[M[+x]](t: [A] => F[A] => M[A])(using M: Monad[M]): M[A] =
        M.recursion(this) {
            case Pure(a)     => M.pure(Right(a))
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


@main def check() = 
    def go(x: Int, trace: Int = 0): Free[[a] =>> (a, a), Int] = 
        if x == 0 then Free.Pure(trace)
        else if x % 1000 == 0 then 
            Free.Embed[[a] =>> (a, a), Int]((1, 0)).flatMap(i => go(x - 1, trace * 10 + i))
        else Free.Pure(()).flatMap(_ => go(x - 1, trace))

    val res = go(10000).foldMap([A] => (x: (A, A)) => Vector(x._1, x._2))

    println(Free.Embed(List(1, 2, 3)).foldMap([A] => (x: List[A]) => x))
    println(res)