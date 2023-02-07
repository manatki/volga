package volga.functors

import compiletime.*
import deriving.Mirror.{ProductOf, SumOf}
import scala.reflect.TypeTest

private def subtypeToIntersectionEq[A, B](using ev: A <:< B): A =:= (A & B) =
    given (A <:< (A & B)) = ev.liftCo[[x] =>> A & x]
    <:<.antisymm

private def tupleFromProduct[A](a: A)(using m: ProductOf[A])(using A <:< Product): m.MirroredElemTypes =
    val coerce = subtypeToIntersectionEq[A, Product]
    type MP[a] = ProductOf[a] { type MirroredElemTypes = m.MirroredElemTypes }
    val coeMirror = coerce.liftCo[MP](m)
    Tuple.fromProductTyped[A & Product](coerce(a))(using coeMirror)
end tupleFromProduct
trait Functor[F[_]]:
    def covariant[A, B](using A <:< B): F[A] <:< F[B]
    extension [A](fa: F[A])
        def map[B](f: A => B): F[B]
        def widen[B](using ev: A <:< B): F[B] = covariant[A, B](fa)

object Functor:

    trait Cov[F[+_]] extends Functor[F]:
        def covariant[A, B](using ev: A <:< B): F[A] <:< F[B] = ev.liftCo[F]

    inline def derived[F[+_]]: Cov[F] = new:
        extension [A](fa: F[A]) def map[B](f: A => B): F[B] = functorCall[A, B, F[A], F[B]](fa, f)

    inline def functorCall[A, B, FA, FB](fa: FA, f: A => B): FB =
        summonFrom {
            case ma: ProductOf[FA] =>
                type TA = ma.MirroredElemTypes
                summonFrom { case mb: ProductOf[FB] =>
                    type TB = mb.MirroredElemTypes
                    val ta = tupleFromProduct(fa)(using ma)(using summonInline[FA <:< Product])
                    val tb = functorProduct[A, B, TA, TB](ta, f)
                    mb.fromTuple(tb)
                }
            case ma: SumOf[FA]     =>
                type SA = ma.MirroredElemTypes

                summonFrom { case mb: SumOf[FB] =>
                    type SB = mb.MirroredElemTypes
                    functorSum[A, B, FA, FB, SA, SB](fa, f)
                }
        }

    inline def functorProduct[A, B, TA <: Tuple, TB <: Tuple](ta: TA, f: A => B): TB =
        inline ta match
            case EmptyTuple   =>
                summonInline[EmptyTuple <:< TB](EmptyTuple)
            case t: (a *: ta) =>
                inline erasedValue[TB] match
                    case _: (b *: tb) =>
                        val tb: tb = functorProduct[A, B, ta, tb](t.tail, f)
                        val b: b   = summonInline[HeadMatch[a, b, A, B]](t.head)(f)
                        summonInline[(b *: tb) =:= TB](b *: tb)

    inline def functorSum[A, B, FA, FB, SA, SB](pa: FA, f: A => B): FB =
        inline erasedValue[SA] match
            case EmptyTuple   =>
                throw IllegalArgumentException(s"can't match $pa")
            case t: (a *: ta) =>
                type Head = a
                inline erasedValue[SB] match
                    case _: (b *: tb) =>
                        summonFrom { case given TypeTest[FA, Head] =>
                            pa match
                                case h: Head => summonInline[b <:< FB](functorCall[A, B, a, b](h, f))
                                case _       => functorSum[A, B, FA, FB, ta, tb](pa, f)
                        }

    trait TraverseMonoidal[F[+_]] extends Traverse.Cov[F], Monoidal[F]

    given TraverseMonoidal[[x] =>> x] with
        def pure[A](x: A): A = x
        extension [A](a: A)
            override def map[B](f: A => B): B             = f(a)
            def map2[B, C](b: => B)(f: (A, B) => C)       = f(a, b)
            def traverse[M[_]: Monoidal, B](f: A => M[B]) = f(a)

    given TraverseMonoidal[List] with
        extension [A](fa: List[A])
            override def map2[B, C](fb: => List[B])(f: (A, B) => C): List[C]               =
                for a <- fa; b <- fb yield f(a, b)
            override def traverse[M[_], B](f: A => M[B])(using M: Monoidal[M]): M[List[B]] =
                fa match
                    case Nil     => M.pure(Nil)
                    case a :: ta => f(a).map2(ta.traverse(f))(_ :: _)
        def pure[A](x: A) = x :: Nil
    end given

    trait HeadMatch[AX, BX, A, B]:
        def apply(ax: AX)(f: A => B): BX

    object HeadMatch:

        transparent trait Primary
        given [A, B]: HeadMatch[A, B, A, B] with Primary with
            def apply(a: A)(f: A => B) = f(a)
        given [A, B, X]: HeadMatch[X, X, A, B] with
            def apply(x: X)(f: A => B): X = x

        given [A, B, F[_]: Functor]: HeadMatch[F[A], F[B], A, B] with
            def apply(a: F[A])(f: A => B) = a.map(f)
    end HeadMatch

end Functor
