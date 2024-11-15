package volga.functors

import compiletime.*
import deriving.Mirror.{ProductOf, SumOf}
import scala.reflect.TypeTest

trait Traverse[F[+_]] extends Functor[F]:
    self =>
    extension [A](fa: F[A])
        def traverse[M[+_], B](f: A => M[B])(using Monoidal[M]): M[F[B]]
        override def map[B](f: A => B): F[B] = traverse[[x] =>> x, B](f)

        def mapAccumErr[B, C, E](b: B)(f: (B, A) => Either[E, (B, C)]): Either[E, (B, F[C])] =
            def mod(b: B, a: A): (B, State[B, E, C]) = f(b, a) match
                case Left(e)        => (b, State.Error(e))
                case Right((b1, c)) => (b1, State.Success(c))

            def from(a: A) = State.Modify(mod(_, a)).flatten

            traverse(from).run(b) match
                case (b, State.Success(c)) => Right((b, c))
                case (b, State.Error(e))   => Left(e)

        end mapAccumErr
    end extension

    extension [A, M[+_]](fa: F[M[A]]) def sequence(using Monoidal[M]): M[F[A]] = fa.traverse(x => x)

    given composeTraverse[G[+_]: Traverse]: Traverse[[x] =>> F[G[x]]] with
        extension [A](fga: F[G[A]])
            def traverse[M[+_], B](f: A => M[B])(using Monoidal[M]): M[F[G[B]]] =
                self.traverse(fga)(_.traverse(f))
end Traverse

object Traverse:

    inline def derived[F[+_]]: Traverse[F] = new:
        extension [A](fa: F[A])
            def traverse[M[+_]: Monoidal, B](f: A => M[B]): M[F[B]] =
                traverseCall[A, B, F[A], F[B], M](fa, f)

            override def map[B](f: A => B): F[B] =
                Functor.functorCall[A, B, F[A], F[B]](fa, f)

    inline def traverseCall[A, B, FA, FB, M[+_]: Monoidal](fa: FA, f: A => M[B]): M[FB] =
        summonFrom {
            case ma: ProductOf[FA] =>
                type TA = ma.MirroredElemTypes
                summonFrom { case mb: ProductOf[FB] =>
                    type TB = mb.MirroredElemTypes
                    val ta = tupleFromProduct(fa)(using ma)(using summonInline[FA <:< Product])
                    val tb = traverseProduct[A, B, TA, TB, M](ta, f)
                    tb.map(mb.fromTuple)
                }
            case ma: SumOf[FA]     =>
                type SA = ma.MirroredElemTypes

                summonFrom { case mb: SumOf[FB] =>
                    type SB = mb.MirroredElemTypes
                    traverseSum[A, B, FA, FB, SA, SB, M](fa, f)
                }
        }

    inline def traverseProduct[A, B, TA <: Tuple, TB <: Tuple, M[+_]](t: TA, f: A => M[B])(using
        M: Monoidal[M]
    ): M[TB] =
        inline t match
            case _: EmptyTuple =>
                M.pure(summonInline[EmptyTuple <:< TB](EmptyTuple))
            case t: (a *: ta)  =>
                val t1: (a *: ta) = t
                inline erasedValue[TB] match
                    case _: (b *: tb) =>
                        val mtb: M[tb] = traverseProduct[A, B, ta, tb, M](t1.tail, f)
                        val mb: M[b]   = summonInline[HeadMatch[a, b, A, B, M]](t1.head)(f)
                        mb.map2(mtb) { (b, tb) => summonInline[(b *: tb) =:= TB](b *: tb) }

    inline def traverseSum[A, B, FA, FB, SA, SB, M[+_]: Monoidal](pa: FA, f: A => M[B]): M[FB] =
        inline erasedValue[SA] match
            case _: EmptyTuple =>
                throw IllegalArgumentException(s"can't match $pa")
            case t: (a *: ta)  =>
                type Head = a
                inline erasedValue[SB] match
                    case _: (b *: tb) =>
                        summonFrom { case given TypeTest[FA, Head] =>
                            pa match
                                case h: Head =>
                                    summonInline[M[b] <:< M[FB]](traverseCall[A, B, a, b, M](h, f))
                                case _       => traverseSum[A, B, FA, FB, ta, tb, M](pa, f)
                        }

    trait HeadMatch[AX, BX, A, B, M[+_]]:
        def apply(ax: AX)(f: A => M[B])(using M: Monoidal[M]): M[BX]

    object HeadMatch:
        transparent trait Primary

        given here[A, B, M[+_]]: HeadMatch[A, B, A, B, M] with Primary with
            def apply(a: A)(f: A => M[B])(using Monoidal[M]) = f(a)

        given ignore[A, B, X, M[+_]]: HeadMatch[X, X, A, B, M] with
            def apply(x: X)(f: A => M[B])(using M: Monoidal[M]): M[X] = M.pure(x)

        given single[A, B, F[+_]: Traverse, M[+_]]: HeadMatch[F[A], F[B], A, B, M] with
            def apply(a: F[A])(f: A => M[B])(using Monoidal[M]) = a.traverse(f)

        given double[A, B, F[+_]: Traverse, G[+_]: Traverse, M[+_]]: HeadMatch[F[G[A]], F[G[B]], A, B, M] with
            def apply(a: F[G[A]])(f: A => M[B])(using Monoidal[M]) = a.traverse(_.traverse(f))
    end HeadMatch

end Traverse
