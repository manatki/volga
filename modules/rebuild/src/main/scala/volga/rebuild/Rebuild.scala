package volga.rebuild
import cats.{Applicative, Functor, Monad}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._
import volga.Arr
import volga.rebuild.Rebuild.Identity

trait Rebuild[F[_], A, B] { self =>
  type I
  def build(a: A)(implicit F: Monad[F]): F[(I, B)]
  def rebuild(a: A, refresh: Boolean, mid: I)(implicit F: Monad[F]): F[(I, B, Boolean)]

  def andThen[C](that: Rebuild[F, B, C]): Rebuild[F, A, C] =
    that match {
      case lifted: LiftedRebuild[F, B, C] => self.rmap(lifted)
      case _ =>
        new Rebuild[F, A, C] {
          type I = (self.I, that.I)
          def build(a: A)(implicit F: Monad[F]): F[(I, C)] =
            for {
              (i1, b) <- self.build(a)
              (i2, c) <- that.build(b)
            } yield ((i1, i2), c)
          def rebuild(a: A, refresh: Boolean, mid: I)(implicit F: Monad[F]): F[(I, C, Boolean)] =
            for {
              (i1, b, r1) <- self.rebuild(a, refresh, mid._1)
              (i2, c, r2) <- that.rebuild(b, r1, mid._2)
            } yield ((i1, i2), c, r2)
        }
    }

  def split[C, D](that: Rebuild[F, C, D]): Rebuild[F, (A, C), (B, D)] =
    new Rebuild[F, (A, C), (B, D)] {
      type I = (self.I, that.I)
      def build(ac: (A, C))(implicit F: Monad[F]): F[(I, (B, D))] =
        self.build(ac._1).map2(that.build(ac._2)) { case ((i1, b), (i2, d)) => ((i1, i2), (b, d)) }

      def rebuild(ac: (A, C), refresh: Boolean, mid: I)(implicit F: Monad[F]): F[(I, (B, D), Boolean)] =
        self.rebuild(ac._1, refresh, mid._1).map2(that.rebuild(ac._2, refresh, mid._2)) {
          case ((i1, b, r1), (i2, d, r2)) => ((i1, i2), (b, d), r1 || r2)
        }
    }

  def bimap[C, D](f: C => A)(g: B => D): Rebuild[F, C, D] =
    new Rebuild[F, C, D] {
      type I = self.I
      def build(c: C)(implicit F: Monad[F]): F[(I, D)] =
        self.build(f(c)).map { case (i, b) => (i, g(b)) }
      def rebuild(c: C, refresh: Boolean, mid: I)(implicit F: Monad[F]): F[(I, D, Boolean)] =
        self.rebuild(f(c), refresh, mid).map { case (i, b, r) => (i, g(b), r) }
    }

  def lmap[C](f: C => A): Rebuild[F, C, B] = bimap(f)(identity)
  def rmap[C](f: B => C): Rebuild[F, A, C] = bimap[A, C](identity)(f)
}

trait LiftedRebuild[F[_], A, B] extends Rebuild[F, A, B] with (A => B) { self =>
  def apply(a: A): B
  type I = Unit
  def build(a: A)(implicit F: Monad[F]): F[(Unit, B)] = F.pure(((), apply(a)))
  def rebuild(a: A, refresh: Boolean, mid: Unit)(implicit F: Monad[F]): F[(Unit, B, Boolean)] =
    F.pure(((), apply(a), refresh))

  override def andThen[C](g: Rebuild[F, B, C]): Rebuild[F, A, C] = g match {
    case lifted: LiftedRebuild[F, B, C] => Rebuild.lift[F]((a: A) => lifted(apply(a)))
    case _                              => g.lmap(this)
  }

  override def split[C, D](that: Rebuild[F, C, D]): Rebuild[F, (A, C), (B, D)] = that match {
    case lifted: LiftedRebuild[F, C, D] => Rebuild.lift[F] { case (a, c) => (apply(a), lifted(c)) }
    case _ =>
      new Rebuild[F, (A, C), (B, D)] {
        type I = that.I

        def build(ac: (A, C))(implicit F: Monad[F]): F[(I, (B, D))] =
          that.build(ac._2).map { case (i, d) => (i, (self(ac._1), d)) }

        def rebuild(ac: (A, C), refresh: Boolean, mid: I)(implicit F: Monad[F]): F[(I, (B, D), Boolean)] =
          that.rebuild(ac._2, refresh, mid).map { case (next, d, re) => (next, (self(ac._1), d), re) }
      }
  }
}

object Rebuild {
  type AnyK[+A] <: Any
  private case object Identity extends LiftedRebuild[AnyK, Any, Any] {
    def apply(x: Any) = x
  }

  def id[F[_], A]: Rebuild[F, A, A] = Identity.asInstanceOf[Rebuild[F, A, A]]

  def lift[F[_]] = new MkLift[F](true)

  class MkLift[F[_]](val dummy: Boolean) extends AnyVal {
    def apply[A, B](f: LiftedRebuild[F, A, B]): Rebuild[F, A, B] = f
  }

  implicit def arr[F[_]: Monad]: Arr[Rebuild[F, *, *]] = new Arr[Rebuild[F, *, *]] {
    def lift[A, B](f: A => B): Rebuild[F, A, B] = lift(f)

    def split[A, B, C, D](f: Rebuild[F, A, C], g: Rebuild[F, B, D]): Rebuild[F, (A, B), (C, D)] = f.split(g)

    def compose[A, B, C](f: Rebuild[F, B, C], g: Rebuild[F, A, B]): Rebuild[F, A, C] = g.andThen(f)
  }

}
