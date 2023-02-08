package volga.functors
package instances

given vectorFunctor: TraverseMonad[Vector] with
    override def rightAssoc: Boolean                                                    = false
    extension [A](fa: Vector[A])
        override def flatMap[B](f: A => Vector[B]): Vector[B]                             = fa.flatMap(f)
        override def traverse[M[+_], B](f: A => M[B])(using M: Monoidal[M]): M[Vector[B]] =
            M.tabulateTraverse(fa.length)(i => f(fa(i))).map(_.toVector)
    def pure[A](x: A)                                                                   = Vector(x)
    override def recursion[A, B](a: A)(f: A => Vector[Either[A, B]]): Vector[B] =
        Monad.collectionRecursion(Vector, a, f)
end vectorFunctor
