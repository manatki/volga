package volga.functors
package instances

given listFunctor: TraverseMonoidal[List] with
    extension [A](fa: List[A])
        override def map2[B, C](fb: => List[B])(f: (A, B) => C): List[C]               =
            for a <- fa; b <- fb yield f(a, b)
        override def traverse[M[_], B](f: A => M[B])(using M: Monoidal[M]): M[List[B]] =
            fa match
                case Nil     => M.pure(Nil)
                case a :: ta => f(a).map2(ta.traverse(f))(_ :: _)
    def pure[A](x: A) = x :: Nil
end listFunctor
