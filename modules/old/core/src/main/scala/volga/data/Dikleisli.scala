package volga.data

import cats.data.AndThen
import cats.instances.tuple._
import cats.syntax.all._
import cats.{Applicative, Monad}
import volga.Symon
import volga.control.MPar

trait Dikleisli[F[_], G[_], A, B] {
  def to(a: A): F[B]
  def from(b: B): G[A]
}

object Dikleisli {
  def apply[F[_], G[_], A, B](f: A => F[B])(g: B => G[A]): Dikleisli[F, G, A, B] = new Dikleisli[F, G, A, B] {
    def to(a: A): F[B]   = f(a)
    def from(b: B): G[A] = g(b)
  }

  def lift[F[_], G[_]] = new LiftApplied[F, G](true)

  class LiftApplied[F[_], G[_]](private val __ : Boolean) extends AnyVal {
    def apply[A, B](f: A => B)(g: B => A)(implicit F: Applicative[F], G: Applicative[G]): Dikleisli[F, G, A, B] =
      new Dikleisli[F, G, A, B] {
        def to(a: A): F[B]   = f(a).pure[F]
        def from(b: B): G[A] = g(b).pure[G]
      }
  }

  implicit def instance[F[_]: MPar.TC: Monad, G[_]: MPar.TC: Monad]: DikleisliSymon[F, G] = new DikleisliSymon[F, G]

  class DikleisliSymon[F[_]: MPar.TC: Monad, G[_]: MPar.TC: Monad] extends Symon[Dikleisli[F, G, *, *], (*, *), Unit] {
    @inline private def lifted[A, B](f: A => B)(g: B => A): Dikleisli[F, G, A, B] = lift[F, G](f)(g)

    private[volga] type _a
    private[volga] type _b
    private[volga] type _c

    private case class Pure[A, B](f: A => B)(val g: B => A) extends Dikleisli[F, G, A, B] {
      def to(a: A): F[B]   = f(a).pure[F]
      def from(b: B): G[A] = g(b).pure[G]

      def pandThen[C](pure: Pure[B, C]): Pure[A, C] =
        Pure[A, C](AndThen(f).andThen(pure.f))(AndThen(pure.g).andThen(g))

      def psplit[C, D](pure: Pure[C, D]): Pure[(A, C), (B, D)] =
        Pure[(A, C), (B, D)] { case (a, c) => (f(a), pure.f(c)) } { case (b, d) => (g(b), pure.g(d)) }
    }

    private[this] val swapAny = Pure[(_a, _b), (_b, _a)](_.swap)(_.swap)

    def swap[A, B] = swapAny.asInstanceOf[Dikleisli[F, G, (A, B), (B, A)]]

    private[this] val lUnitAny = Pure[(Unit, _a), _a](_._2)(((), _))

    def lunit[A] = lUnitAny.asInstanceOf[Dikleisli[F, G, (Unit, A), A]]

    private[this] val unitLAny = Pure[_a, (Unit, _a)](((), _))(_._2)

    def unitl[A] = unitLAny.asInstanceOf[Dikleisli[F, G, A, (Unit, A)]]

    private[this] val rUnitAny = Pure[(_a, Unit), _a](_._1)((_, ()))

    override def runit[A] = rUnitAny.asInstanceOf[Dikleisli[F, G, (A, Unit), A]]

    private[this] val unitRAny = Pure[_a, (_a, Unit)]((_, ()))(_._1)

    override def unitr[A] = unitRAny.asInstanceOf[Dikleisli[F, G, A, (A, Unit)]]

    private[this] val assocLAny =
      Pure[(_a, (_b, _c)), ((_a, _b), _c)] { case (a, (b, c)) => ((a, b), c) } { case ((a, b), c) => (a, (b, c)) }

    def assocl[A, B, C] = assocLAny.asInstanceOf[Dikleisli[F, G, (A, (B, C)), ((A, B), C)]]

    private[this] val assocRAny =
      Pure[((_a, _b), _c), (_a, (_b, _c))] { case ((a, b), c) => (a, (b, c)) } { case (a, (b, c)) => ((a, b), c) }

    override def assocr[A, B, C] = assocRAny.asInstanceOf[Dikleisli[F, G, ((A, B), C), (A, (B, C))]]

    private[this] val idAny = Pure[_a, _a](a => a)(a => a)

    def id[A]: Dikleisli[F, G, A, A] = idAny.asInstanceOf[Dikleisli[F, G, A, A]]

    @inline private[this] def tensorDefault[A, B, C, D](f: Dikleisli[F, G, A, B], g: Dikleisli[F, G, C, D]) =
      new Dikleisli[F, G, (A, C), (B, D)] {
        def to(a: (A, C)): F[(B, D)] = a.parBitraverse(f.to, g.to)

        def from(b: (B, D)): G[(A, C)] = b.parBitraverse(f.from, g.from)
      }

    def tensor[A, B, C, D](f: Dikleisli[F, G, A, B], g: Dikleisli[F, G, C, D]): Dikleisli[F, G, (A, C), (B, D)] =
      f match {
        case fp: Pure[A, B] =>
          g match {
            case gp: Pure[C, D] => fp.psplit(gp)
            case _              => tensorDefault(f, g)
          }
        case _ => tensorDefault(f, g)
      }

    @inline private[this] def composeDefault[A, B, C](
        f: Dikleisli[F, G, B, C],
        g: Dikleisli[F, G, A, B]
    ): Dikleisli[F, G, A, C] =
      new Dikleisli[F, G, A, C] {
        def to(a: A): F[C] = g.to(a).flatMap(f.to)

        def from(b: C): G[A] = f.from(b).flatMap(g.from)
      }

    def compose[A, B, C](f: Dikleisli[F, G, B, C], g: Dikleisli[F, G, A, B]): Dikleisli[F, G, A, C] = f match {
      case fp: Pure[B, C] =>
        g match {
          case gp: Pure[A, B] => gp.pandThen(fp)
          case _              => composeDefault(f, g)
        }
      case _ => composeDefault(f, g
      )
    }
  }
}
