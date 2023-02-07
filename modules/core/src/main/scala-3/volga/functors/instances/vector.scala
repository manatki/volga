package volga.functors
package instances

given vectorFunctor: TraverseMonoidal[Vector] with
    extension [A](fa: Vector[A])
        override def map2[B, C](fb: => Vector[B])(f: (A, B) => C): Vector[C]             =
            for a <- fa; b <- fb yield f(a, b)
        override def traverse[M[_], B](f: A => M[B])(using M: Monoidal[M]): M[Vector[B]] =
            fa match
                case a +: ta => f(a).map2(ta.traverse(f))(_ +: _)
                case _       => M.pure(Vector.empty)
    def pure[A](x: A) = Vector(x)
end vectorFunctor
