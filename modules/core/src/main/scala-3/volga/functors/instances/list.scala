package volga.functors
package instances

given listFunctor: TraverseMonad[List] with
    extension [A](fa: List[A])
        override def flatMap[B](f: A => List[B]): List[B]                              = fa.flatMap(f)
        override def traverse[M[+_], B](f: A => M[B])(using M: Monoidal[M]): M[List[B]] =
            fa match
                case Nil     => M.pure(Nil)
                case a :: ta => f(a).map2(ta.traverse(f))(_ :: _)
    def pure[A](x: A) = x :: Nil
end listFunctor
