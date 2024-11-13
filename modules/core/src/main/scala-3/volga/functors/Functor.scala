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
trait Functor[F[+_]]:
    self =>

    extension [A](fa: F[A]) def map[B](f: A => B): F[B]

    given composeFunctor[G[+_]: Functor]: Functor[[x] =>> F[G[x]]] with
        extension [A](fga: F[G[A]]) def map[B](f: A => B): F[G[B]] = self.map(fga)(_.map(f))

end Functor

object Functor:

    inline def derived[F[+_]]: Functor[F] = new:
        extension [A](fa: F[A]) def map[B](f: A => B): F[B] = functorCall[A, B, F[A], F[B]](fa, f)

    export instances.{vectorFunctor, idFunctor, listFunctor, functionMonad, eitherMonad}

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
            case _: EmptyTuple =>
                summonInline[EmptyTuple <:< TB](EmptyTuple)
            case t: (a *: ta)  =>
                val (th *: tt) = (t: a *: ta)
                inline erasedValue[TB] match
                    case _: (b *: tb) =>
                        val tb: tb = functorProduct[A, B, ta, tb](tt, f)
                        val b: b   = summonInline[HeadMatch[a, b, A, B]](th)(f)
                        summonInline[(b *: tb) =:= TB](b *: tb)

    inline def functorSum[A, B, FA, FB, SA, SB](pa: FA, f: A => B): FB =
        inline erasedValue[SA] match
            case _: EmptyTuple =>
                throw IllegalArgumentException(s"can't match $pa")
            case t: (a *: ta)  =>
                type Head = a
                inline erasedValue[SB] match
                    case _: (b *: tb) =>
                        summonFrom { case given TypeTest[FA, Head] =>
                            pa match
                                case h: Head => summonInline[b <:< FB](functorCall[A, B, a, b](h, f))
                                case _       => functorSum[A, B, FA, FB, ta, tb](pa, f)
                        }

    trait HeadMatch[AX, BX, A, B]:
        def apply(ax: AX)(f: A => B): BX

    object HeadMatch:

        transparent trait Primary
        given here[A, B]: HeadMatch[A, B, A, B] with Primary with
            def apply(a: A)(f: A => B) = f(a)
        given ignore[A, B, X]: HeadMatch[X, X, A, B] with
            def apply(x: X)(f: A => B): X = x

        given single[A, B, F[+_]: Functor]: HeadMatch[F[A], F[B], A, B] with
            def apply(a: F[A])(f: A => B) = a.map(f)

        given double[A, B, F[+_]: Functor, G[+_]: Functor]: HeadMatch[F[G[A]], F[G[B]], A, B] with
            def apply(a: F[G[A]])(f: A => B) = a.map(_.map(f))
    end HeadMatch

end Functor
