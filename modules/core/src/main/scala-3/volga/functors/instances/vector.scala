package volga.functors
package instances

given vectorFunctor: TraverseMonad[Vector] with
    extension [A](fa: Vector[A])
        override def flatMap[B](f: A => Vector[B]): Vector[B]                            = fa.flatMap(f)
        override def traverse[M[+_], B](f: A => M[B])(using M: Monoidal[M]): M[Vector[B]] =
            fa match
                case a +: ta => f(a).map2(ta.traverse(f))(_ +: _)
                case _       => M.pure(Vector.empty)
    def pure[A](x: A) = Vector(x)
end vectorFunctor
