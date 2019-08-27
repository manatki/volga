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
  implicit val symon: Symon[Iso, (?, ?), Unit] = new Symon[Iso, (?, ?), Unit] {
    def swap[A, B]: Iso[(A, B), (B, A)] = new Iso[(A, B), (B, A)] {
      def to(a: (A, B)): (B, A)   = a.swap
      def from(b: (B, A)): (A, B) = b.swap
    }

    def lunit[A]: Iso[(Unit, A), A] = new Iso[(Unit, A), A] {
      def to(a: (Unit, A)): A   = a._2
      def from(b: A): (Unit, A) = ((), b)
    }
    def unitl[A]: Iso[A, (Unit, A)] = lunit.invert
    def assocl[A, B, C]: Iso[(A, (B, C)), ((A, B), C)] = new Iso[(A, (B, C)), ((A, B), C)] {
      def to(a: (A, (B, C))): ((A, B), C)   = ((a._1, a._2._1), a._2._2)
      def from(b: ((A, B), C)): (A, (B, C)) = (b._1._1, (b._1._2, b._2))
    }
    def id[A]: Iso[A, A] = new Iso[A, A] {
      def to(a: A): A   = a
      def from(b: A): A = b
    }
    def tensor[A, B, C, D](f: Iso[A, B], g: Iso[C, D]) = new Iso[(A, C), (B, D)] {
      def to(a: (A, C)): (B, D)   = (f.to(a._1), g.to(a._2))
      def from(b: (B, D)): (A, C) = (f.from(b._1), g.from(b._2))
    }
    def compose[A, B, C](f: Iso[B, C], g: Iso[A, B]): Iso[A, C] =
      new Iso[A, C] {
        def to(a: A): C   = f.to(g.to(a))
        def from(b: C): A = g.from(f.from(b))
      }
  }
}
