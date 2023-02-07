package volga.functors

import compiletime.*
import deriving.Mirror.{ProductOf, SumOf}

trait Functor[F[_]]:
    extension [A](fa: F[A]) def map[B](f: A => B): F[B]

object Functor:
    inline given derived[T[_] <: Product]: Functor[T] = new:
        extension [A](fa: T[A])
            def map[B](f: A => B): T[B] =
                summonFrom { case ma: ProductOf[T[A]] =>
                    type TA = ma.MirroredElemTypes
                    summonFrom { case mb: ProductOf[T[B]] =>
                        type TB = mb.MirroredElemTypes
                        val ta = Tuple.fromProductTyped(fa)(using ma)
                        val tb = functorCall[A, B, TA, TB](ta, f)
                        mb.fromTuple(tb)
                    }
                }

    inline def functorCall[A, B, TA <: Tuple, TB <: Tuple](t: TA, f: A => B): TB =
        inline t match
            case EmptyTuple   =>
                summonInline[EmptyTuple <:< TB](EmptyTuple)
            case t: (a *: ta) =>
                inline erasedValue[TB] match
                    case _: (b *: tb) =>
                        val tb: tb = functorCall[A, B, ta, tb](t.tail, f)
                        val b: b   = summonInline[HeadMatch[a, b, A, B]](t.head)(f)
                        summonInline[(b *: tb) =:= TB](b *: tb)

    given Traverse[[x] =>> x] with Monoidal[[x] =>> x] with
        def pure[A](x: A): A = x
        extension [A](a: A)
            override def map[B](f: A => B): B             = f(a)
            def map2[B, C](b: B)(f: (A, B) => C)          = f(a, b)
            def traverse[M[_]: Monoidal, B](f: A => M[B]) = f(a)

    trait HeadMatch[AX, BX, A, B]:
        def apply(ax: AX)(f: A => B): BX

    object HeadMatch:
        given [A, B]: HeadMatch[A, B, A, B] with
            def apply(a: A)(f: A => B) = f(a)
        given [A, B, X]: HeadMatch[X, X, A, B] with
            def apply(x: X)(f: A => B): X = x
end Functor

trait Monoidal[F[_]] extends Functor[F]:
    def pure[A](a: A): F[A]
    extension [A](fa: F[A])
        def map2[B, C](fb: F[B])(f: (A, B) => C): F[C]

        def map[B](f: A => B): F[B] = fa.map2(pure(()))((a, _) => f(a))

trait Traverse[F[_]] extends Functor[F]:
    extension [A](fa: F[A])
        def traverse[M[_], B](f: A => M[B])(using Monoidal[M]): M[F[B]]
        def map[B](f: A => B): F[B] = traverse[[x] =>> x, B](f)

object Traverse:
    // inline given derived[T[_] <: Product]: Functor[T] = new:
    //     extension [A](fa: T[A])
    //         def map[B](f: A => B): T[B] =
    //             summonFrom { case ma: ProductOf[T[A]] =>
    //                 type TA = ma.MirroredElemTypes
    //                 summonFrom { case mb: ProductOf[T[B]] =>
    //                     type TB = mb.MirroredElemTypes
    //                     val ta = Tuple.fromProductTyped(fa)(using ma)
    //                     val tb = functorCall[A, B, TA, TB](ta, f)
    //                     mb.fromTuple(tb)
    //                 }
    //             }

    inline def traverseCall[A, B, TA <: Tuple, TB <: Tuple, M[_]](t: TA, f: A => M[B])(using M: Monoidal[M]): M[TB] =
        inline t match
            case EmptyTuple   =>
                M.pure(summonInline[EmptyTuple <:< TB](EmptyTuple))
            case t: (a *: ta) =>
                inline erasedValue[TB] match
                    case _: (b *: tb) =>
                        val mtb: M[tb] = traverseCall[A, B, ta, tb, M](t.tail, f)
                        val mb: M[b]   = summonInline[HeadMatch[a, b, A, B, M]](t.head)(f)
                        mb.map2(mtb)((b, tb) => summonInline[(b *: tb) =:= TB](b *: tb))

    trait HeadMatch[AX, BX, A, B, M[_]]:
        def apply(ax: AX)(f: A => M[B]): M[BX]

    object HeadMatch:
        given [A, B, M[_]]: HeadMatch[A, B, A, B, M[_]] with
            def apply(a: A)(f: A => M[B]) = f(a)
        given [A, B, X, M[_]](using M: Monoidal[M]): HeadMatch[X, X, A, B, M] with
            def apply(x: X)(f: A => M[B]): M[X] = M.pure(x)
end Traverse
