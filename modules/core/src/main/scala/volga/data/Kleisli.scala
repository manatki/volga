package volga.data
import cats.Monad
import volga.ArrApply
import tofu.syntax.monadic._
import cats.syntax.bitraverse._
import cats.syntax.traverse._
import cats.instances.either._

trait Kleisli[+F[+_], -A, +B] {
  def run(a: A): F[B]
}

object Kleisli {
  implicit def arrowInstance[F[+_]: Monad]: ArrApply[Kleisli[F, *, *]] = new ArrApply[Kleisli[F, *, *]] {
    final def app[A, B]: Kleisli[F, (A, Kleisli[F, A, B]), B] = { case (a, fab) => fab.run(a) }
    final def lift[A, B](f: A => B): Kleisli[F, A, B]         = f(_).pure[F]

    final def split[A, B, C, D](f: Kleisli[F, A, C], g: Kleisli[F, B, D]): Kleisli[F, (A, B), (C, D)] = {
      case (a, b) => f.run(a).map2(g.run(b))((_, _))
    }

    final def compose[A, B, C](f: Kleisli[F, B, C], g: Kleisli[F, A, B]): Kleisli[F, A, C] =
      g.run(_).flatMap(f.run)

    final override def choose[A, B, C, D](
        f: Kleisli[F, A, C]
    )(g: Kleisli[F, B, D]): Kleisli[F, Either[A, B], Either[C, D]] =
      _.bitraverse(f.run, g.run)

    final override def choice[A, B, C](f: Kleisli[F, A, C])(g: Kleisli[F, B, C]): Kleisli[F, Either[A, B], C] =
      _.fold(f.run, g.run)

    final override def left[A, B, C](fab: Kleisli[F, A, B]): Kleisli[F, Either[A, C], Either[B, C]] =
      _.leftTraverse(fab.run)

    final override def right[A, B, C](fab: Kleisli[F, A, B]): Kleisli[F, Either[C, A], Either[C, B]] =
      _.traverse(fab.run)

    final override def product[A, B, C](f: Kleisli[F, A, B], g: Kleisli[F, A, C]): Kleisli[F, A, (B, C)] =
      a => f.run(a).map2(g.run(a))((_, _))

    final override def term[A]: Kleisli[F, A, Unit] = _ => unit[F]

    final override def first[A, B, C](f: Kleisli[F, A, B]): Kleisli[F, (A, C), (B, C)] = {
      case (a, c) => f.run(a).tupleRight(c)
    }
    final override def second[A, B, C](f: Kleisli[F, A, B]): Kleisli[F, (C, A), (C, B)] = {
      case (c, a) => f.run(a).tupleLeft(c)
    }
    final override def product3[A, B1, B2, B3](
        a1: Kleisli[F, A, B1],
        a2: Kleisli[F, A, B2],
        a3: Kleisli[F, A, B3]
    ): Kleisli[F, A, (B1, B2, B3)] =
      a => (a1.run(a), a2.run(a), a3.run(a)).tupled

    final override def product4[A, B1, B2, B3, B4](
        a1: Kleisli[F, A, B1],
        a2: Kleisli[F, A, B2],
        a3: Kleisli[F, A, B3],
        a4: Kleisli[F, A, B4]
    ): Kleisli[F, A, (B1, B2, B3, B4)] =
      a => (a1.run(a), a2.run(a), a3.run(a), a4.run(a)).tupled

    final override def product5[A, B1, B2, B3, B4, B5](
        a1: Kleisli[F, A, B1],
        a2: Kleisli[F, A, B2],
        a3: Kleisli[F, A, B3],
        a4: Kleisli[F, A, B4],
        a5: Kleisli[F, A, B5]
    ): Kleisli[F, A, (B1, B2, B3, B4, B5)] =
      a => (a1.run(a), a2.run(a), a3.run(a), a4.run(a), a5.run(a)).tupled

    final override def mergeMap2[A, B1, B2, C](a1: Kleisli[F, A, B1], a2: Kleisli[F, A, B2])(
        f: (B1, B2) => C
    ): Kleisli[F, A, C] =
      a => a1.run(a).map2(a2.run(a))(f)

    final override def mergeMap3[A, B1, B2, B3, C](a1: Kleisli[F, A, B1], a2: Kleisli[F, A, B2], a3: Kleisli[F, A, B3])(
        f: (B1, B2, B3) => C
    ): Kleisli[F, A, C] =
      a => (a1.run(a), a2.run(a), a3.run(a)).mapN(f)

    final override def mergeMap4[A, B1, B2, B3, B4, C](
        a1: Kleisli[F, A, B1],
        a2: Kleisli[F, A, B2],
        a3: Kleisli[F, A, B3],
        a4: Kleisli[F, A, B4]
    )(f: (B1, B2, B3, B4) => C): Kleisli[F, A, C] =
      a => (a1.run(a), a2.run(a), a3.run(a), a4.run(a)).mapN(f)

    final override def mergeMap5[A, B1, B2, B3, B4, B5, C](
        a1: Kleisli[F, A, B1],
        a2: Kleisli[F, A, B2],
        a3: Kleisli[F, A, B3],
        a4: Kleisli[F, A, B4],
        a5: Kleisli[F, A, B5]
    )(f: (B1, B2, B3, B4, B5) => C): Kleisli[F, A, C] =
      a => (a1.run(a), a2.run(a), a3.run(a), a4.run(a), a5.run(a)).mapN(f)
  }
}
