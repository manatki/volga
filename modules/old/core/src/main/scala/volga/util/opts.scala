package volga.util
import cats.Apply
import cats.instances.list._
import cats.syntax.apply._
import tofu.optics.{Contains, PContains, PItems, PRepeated, Property}

object opts {
  def both[A, B] = new PRepeated[(A, A), (B, B), A, B] {
    def traverse1[F[+ _]: Apply](s: (A, A))(f: A => F[B]): F[(B, B)] =
      (f(s._1), f(s._2)).tupled
  }
  def listElems[A, B] = PItems.fromTraverse[List, A, B]

  def listAt[A](i: Int): Contains[List[A], Option[A]] =
    Contains[List[A]](_.lift(i)) {
      case (l, None)    => l
      case (l, Some(a)) => l.updated(i, a)
    }

  def mapAt[K, V](k: K) = Contains[Map[K, V]](_.get(k)) {
    case (l, None)    => l - k
    case (l, Some(a)) => l.updated(k, a)
  }

  def listItem[A](i: Int) = Property[List[A]](_.lift(i))(_.updated(i, _))
}
