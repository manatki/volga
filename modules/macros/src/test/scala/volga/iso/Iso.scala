package volga
package iso

trait Iso[A, B] { self =>
  def to(a: A): B
  def from(b: B): A

  def invert: Iso[B, A] = new Iso[B, A] {
    def to(a: B): A                = self.from(a)
    def from(b: A): B              = self.to(b)
    override def invert: Iso[A, B] = self
  }
}

object Iso {
  def apply[A, B](f: A => B)(g: B => A): Iso[A, B] = new Iso[A, B] {
    def to(a: A): B = f(a)

    def from(b: B): A = g(b)
  }

  implicit val symon: Symon[Iso, (*, *), Unit] = new Symon[Iso, (*, *), Unit] {
    def swap[A, B] = Iso[(A, B), (B, A)](_.swap)(_.swap)

    def lunit[A] =
      Iso[(Unit, A), A](_._2)(((), _))

    def unitl[A]: Iso[A, (Unit, A)] = lunit.invert

    def assocl[A, B, C] =
      Iso[(A, (B, C)), ((A, B), C)] { case (a, (b, c)) => ((a, b), c) } { case ((a, b), c) => (a, (b, c)) }

    def id[A] = Iso[A, A](identity)(identity)

    def tensor[A, B, C, D](f: Iso[A, B], g: Iso[C, D]) =
      Iso[(A, C), (B, D)] { case (a, c) => (f.to(a), g.to(c)) } { case (b, d) => (f.from(b), g.from(d)) }

    def compose[A, B, C](f: Iso[B, C], g: Iso[A, B]): Iso[A, C] =
      Iso[A, C](a => f.to(g.to(a)))(b => g.from(f.from(b)))
  }
}
