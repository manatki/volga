package volga.rebuild
import cats.{Applicative, Functor, Monad}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._
import volga.rebuild.Rebuild.Identity

trait Rebuild[F[+ _], -A, +B] { self =>
  type I
  def build(a: A)(implicit F: Monad[F]): F[(I, B)]
  def rebuild(a: A, refresh: Boolean, mid: I)(implicit F: Monad[F]): F[(I, B, Boolean)]

  def compose[C](that: Rebuild[F, B, C]): Rebuild[F, A, C] =
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
    that match {
      case _:IdRebuild => that.split(self).bimap(_.swap)(_.swap)
    }


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

trait LiftedRebuild[F[+ _], -A, +B] extends Rebuild[F, A, B] with (A => B) {
  def apply(a: A): B
  type I = Unit
  def build(a: A)(implicit F: Monad[F]): F[(Unit, B)] = F.pure(((), apply(a)))
  def rebuild(a: A, refresh: Boolean, mid: Unit)(implicit F: Monad[F]): F[(Unit, B, Boolean)] =
    F.pure(((), apply(a), refresh))

  override def compose[C](g: Rebuild[F, B, C]): Rebuild[F, A, C] = g match {
    case lifted: LiftedRebuild[F, B, C] => Rebuild.lift[F]((a: A) => lifted(apply(a)))
    case _                              => g.lmap(this)
  }
}

object Rebuild {
  type AnyK[+A] <: Any
  private abstract class IdRebuild[F[+_], C, D] extends Rebuild[F, C, D]{
    def revSplit[A, B](self: Rebuild[F, A, B])
  }
  private case object Identity extends IdRebuild[AnyK, Any, Any] {
    type I = Any
    def build(a: Any)(implicit F: Monad[AnyK]): AnyK[(Any, Any)] = F.pure((Unit, a))
    def rebuild(a: Any, refresh: Boolean, mid: Identity.I)(implicit F: Monad[AnyK]): AnyK[(Any, Any, Boolean)] =
      F.pure((Unit, a, refresh))
    override def compose[C](that: Rebuild[AnyK, Any, C]): Rebuild[AnyK, Any, C] = that
    override def bimap[C, D](f: C => Any)(g: Any => D): Rebuild[AnyK, C, D]     = lift[AnyK](c => g(f(c)))
    override def split[C, D](that: Rebuild[AnyK, C, D]): Rebuild[AnyK, (Any, C), (Any, D)] =
      new Rebuild[AnyK, (Any, C), (Any, D)] {
        type I = that.I
        def build(a: (Any, C))(implicit F: Monad[AnyK]): AnyK[(I, (Any, D))] = F.map(that.build(a._2)) {
          case (i, a) => (i, ((), a))
        }
        def rebuild(a: (Any, C), refresh: Boolean, mid: that.I)(
            implicit F: Monad[AnyK]): AnyK[(that.I, (Any, D), Boolean)] =
          F.map(that.rebuild(a._2, refresh, mid)) {
            case (i, b, r) => (i, ((), b), r)
          }
      }
  }

  def id[F[+ _], A]: Rebuild[F, A, A] = Identity.asInstanceOf[Rebuild[F, A, A]]

  def lift[F[+ _]] = new MkLift[F](true)

  class MkLift[F[+ _]](val dummy: Boolean) extends AnyVal {
    def apply[A, B](f: LiftedRebuild[F, A, B]): Rebuild[F, A, B] = f
  }

}
