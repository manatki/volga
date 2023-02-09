package volga.functors
package instances

import scala.annotation.tailrec
import volga.functors.Monad.Recur

given listFunctor: TraverseMonad[List] with

    override def rightAssoc: Boolean = false
    extension [A](fa: List[A])
        override def flatMap[B](f: A => List[B]): List[B]                               = fa.flatMap(f)
        override def traverse[M[+_], B](f: A => M[B])(using M: Monoidal[M]): M[List[B]] =
            val values = fa.toVector
            M.tabulateTraverse(values.length)(i => f(values(i)))
    def pure[A](x: A)                = x :: Nil

    override def recursion[A, B](a: A)(f: A => List[Either[A, B]]): List[B] =
        Monad.collectionRecursion(List, a, f)

    override def recur[A, B](a: A)(f: A => Recur[List, A, B]): List[B] = 
        Monad.collectionRecur(List, a, f)

end listFunctor
