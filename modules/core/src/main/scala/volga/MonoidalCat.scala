package volga

import cats.arrow.Category
import cats.Eq
import cats.syntax.eq._

trait Monoid[A] {
  def neutral: A
  def combine(x: A, y: A): A

  implicit def setEq: Eq[A] = Eq.fromUniversalEquals

  class MonoidLaws {
    def left_unit(x: A) =
      combine(neutral, x) == x
    def right_unit(x: A) =
      combine(x, neutral) == x
    def associativity(x: A, y: A, z: A) =
      combine(x, combine(y, z)) == combine(combine(x, y), z)
  }
}

trait Cat[-->[_, _]] {
  def id[A]: A --> A
  def compose[A, B, C](f: B --> C, g: A --> B): A --> C

  implicit def homEq[A, B]: Eq[A --> B] = Eq.fromUniversalEquals

  class CatLaws {
    def left_unit[A, B](f: A --> B) =
      compose(id[B], f) === f
    def right_unit[A, B](f: A --> B) =
      compose(f, id[A]) === f
    def associativity[A, B, C, D](f: A --> B, g: B --> C, h: C --> D) =
      compose(h, compose(g, f)) === compose(compose(h, g), f)
  }

  case class <->[A, B](to: A --> B, from: B --> A) {
    def section    = compose(to, from) === id
    def retraction = compose(from, to) === id
  }
}

trait SemigropalCat[-->[_, _], x[_, _]] extends Category[-->] with Cat[-->] {
  def assocl[A, B, C]: (A x (B x C)) --> ((A x B) x C)
  def assocr[A, B, C]: ((A x B) x C) --> (A x (B x C))

  def assoc[A, B, C]: (A x (B x C)) <-> ((A x B) x C) = <->(assocl, assocr)

  def tensor[A, B, C, D](f: A --> B, g: C --> D): (A x C) --> (B x D)

  class SemigroupalCatLaws {
    def pentagon[A, B, C, D] =
      andThen(
        andThen(tensor(assocr[A, B, C], id[D]), assocr[A, B x C, D]),
        tensor(id[A], assocr[B, C, D])
      ) ===
        andThen(assocr[A x B, C, D], assocr[A, B, C x D])
  }
}

trait MonoidalCat[-->[_, _], x[_, _], I] extends SemigropalCat[-->, x] {
  def lunit[A]: (I x A) --> A
  def unitl[A]: A --> (I x A)
  def runit[A]: (A x I) --> A
  def unitr[A]: A --> (A x I)

  def left_unit[A]: (I x A) <-> A  = <->(lunit, unitl)
  def right_unit[A]: (A x I) <-> A = <->(runit, unitr)

  class MonoidalLaws {
    def triagonal[A, B] =
      assocr[A, I, B] ===
        andThen(tensor(runit[A], id[B]), tensor(id[A], unitl[B]))
  }
}

trait Sym[-->[_, _], x[_, _]] extends SemigropalCat[-->, x] {
  def swap[A, B]: (A x B) --> (B x A)

  def symmetry[A, B]: (A x B) <-> (B x A) = <->(swap, swap)
}

trait Symon[-->[_, _], x[_, _], I] extends Sym[-->, x] with MonoidalCat[-->, x, I]

trait SemiClosed[-->[_, _], x[_, _], ==>[_, _]] extends Sym[-->, x] {
  def lcurry[A, B, C](p: (A x B) --> C): A --> (B ==> C)
  def luncurry[A, B, C](p: A --> (B ==> C)): (A x B) --> C

  def rcurry[A, B, C](p: (A x B) --> C): B --> (A ==> C)   = lcurry(compose(p, swap))
  def runcurry[A, B, C](p: B --> (A ==> C)): (A x B) --> C = compose(luncurry(p), swap)

  def lapply[A, B]: ((A ==> B) x A) --> B = luncurry(id)
  def rapply[A, B]: (A x (A ==> B)) --> B = runcurry(id)

  def lunapply[A, B]: A --> (B ==> (A x B)) = lcurry(id)
  def runapply[A, B]: B --> (A ==> (A x B)) = rcurry(id)
}

trait Closed[-->[_, _], x[_, _], ==>[_, _], I] extends SemiClosed[-->, x, ==>] with MonoidalCat[-->, x, I] {
  def ident[A]: I --> (A ==> A)    = lcurry(lunit)
  def choose[A]: A --> (I ==> A)   = lcurry(runit)
  def unchoose[A]: (I ==> A) --> A = compose(lapply[I, A], unitr[I ==> A])
//  def cmp[A, B, C]: (A ==> B) --> ((C ==> A) ==> (C ==> B)) = ???
}

trait Cartesian[-->[_, _], x[_, _], I] extends Symon[-->, x, I] {
  def proj1[A, B]: (A x B) --> A
  def proj2[A, B]: (A x B) --> B

  def product[A, B, C](f: A --> B, g: A --> C): A --> (B x C)

  def term[A]: A --> I

  def lunit[A]: (I x A) --> A = proj2
  def unitl[A]: A --> (I x A) = product(term, id)
  def runit[A]: (A x I) --> A = proj1
  def unitr[A]: A --> x[A, I] = product(id, term)

  def swap[A, B]: (A x B) --> (B x A) = product(proj2, proj1)

  def assocl[A, B, C]: (A x (B x C)) --> ((A x B) x C) =
    product(product(proj1, compose(proj1[B, C], proj2)), compose(proj2[B, C], proj2))
  def assocr[A, B, C]: ((A x B) x C) --> (A x (B x C)) =
    product(compose(proj1[A, B], proj1), product(compose(proj2[A, B], proj1), proj2))
  def tensor[A, B, C, D](f: A --> B, g: C --> D): (A x C) --> (B x D) =
    product(compose(f, proj1), compose(g, proj2))
}

trait CartesianClosed[-->[_, _], x[_, _], ==>[_, _], I] extends Cartesian[-->, x, I] with Closed[-->, x, ==>, I]
