package volga.pres


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

trait Cat[->[_, _]] {
  def id[A]: A -> A
  def compose[A, B, C](f: B -> C, g: A -> B): A -> C

  implicit def homEq[A, B]: Eq[A -> B] = Eq.fromUniversalEquals

  class CatLaws {
    def left_unit[A, B](f: A -> B) =
      (id[A] >> f) === f
    def right_unit[A, B](f: A -> B) =
      (f >> id[B]) === f
    def associativity[A, B, C, D](f: A -> B, g: B -> C, h: C -> D) =
      (f >> (g >> h)) === ((f >> g) >> h)
  }

  case class <->[A, B](to: A -> B, from: B -> A) {
    def section = (to o from) === id
    def retraction = (from o to) === id
  }

  implicit class CatHomOps[A, B](f: A -> B) {
    def o[C](g: C -> A): C -> B = compose(f, g)
    def >>[C](g: B -> C): A -> C = compose(g, f)
  }
}

trait MonoidalCat[->[_, _], x[_, _], I] extends Cat[->] {
  def left_unit[A]: (I x A) <-> A
  def right_unit[A]: (A x I) <-> A
  def assoc[A, B, C]: (A x (B x C)) <-> ((A x B) x C)
  def ar[A, B, C] = assoc[A, B, C].from

  def tensor[A, B, C, D](f: A -> B, g: C -> D): (A x C) -> (B x D)

  class MonoidalLaws {
    def pentagon[A, B, C, D] =
      ((ar[A, B, C] x id[D]) >> ar[A, B x C, D] >> (id[A] x ar[B, C, D])) ===
        (ar[A x B, C, D] >> ar[A, B, C x D])

    def triagonal[A, B] =
      ar[A, I, B] === ((right_unit[A].to x id[B]) >> (id[A] x left_unit[B].from))

    def tenson_dist[A, B, C, D, E, F]
    (f: A -> B, g: B -> C, h: D -> E, i: E -> F) =
      ((f x h) >> (g x i)) === ((f >> g) x (h >> i))

    def tensor_id[A, B] = (id[A] x id[B]) === id[A x B]
  }

  implicit class TensorHomOps[A, B](f: A -> B) {
    def x[C, D](g: C -> D): (A x C) -> (B x D) = tensor(f, g)
  }
}


trait Symon[->[_, _], x[_, _], I] extends MonoidalCat[->, x, I] {
  def swap[A, B]: (A x B) -> (B x A)

  def symmetry[A, B]: (A x B) <-> (B x A) = <->(swap, swap)

  class SymonLaws {
    def unit_coherence[A] =
      swap[A, I] === (left_unit[A].from o right_unit[A].to)

    def assoc_coherence[A, B, C] =
      ((swap[A, B] x id[C]) >> ar[B, A, C] >> (id[B] x swap[A, C])) ===
        (ar[A, B, C] >> swap[A, B x C] >> ar[B, C, A])
  }
}


trait Closed[->[_, _], x[_, _], ==>[_, _], I] extends Symon[->, x, I] {
  def lcurry[A, B, C](p: (A x B) -> C): A -> (B ==> C)
  def luncurry[A, B, C](p: A -> (B ==> C)): (A x B) -> C

  def rcurry[A, B, C](p: (A x B) -> C): B -> (A ==> C) = lcurry(compose(p, swap))
  def runcurry[A, B, C](p: B -> (A ==> C)): (A x B) -> C = compose(luncurry(p), swap)

  def lapply[A, B]: ((A ==> B) x A) -> B = luncurry(id)
  def rapply[A, B]: (A x (A ==> B)) -> B = runcurry(id)

  def lunapply[A, B]: A -> (B ==> (A x B)) = lcurry(id)
  def runapply[A, B]: B -> (A ==> (A x B)) = rcurry(id)

  def ident[A]: I -> (A ==> A) = lcurry(left_unit[A].to)
  def choose[A]: A -> (I ==> A) = lcurry(right_unit[A].to)
  def unchoose[A]: (I ==> A) -> A = compose(lapply[I, A], right_unit[I ==> A].from)

  class ClosedLaws {
    def curryEq[A, B, C](p: (A x B) -> C) = luncurry(lcurry(p)) === p
    def uncurryEq[A, B, C](p: A -> (B ==> C)) = lcurry(luncurry(p)) === p
  }
}

trait Cartesian[->[_, _], x[_, _], I] extends Symon[->, x, I] {
  def proj1[A, B]: (A x B) -> A
  def proj2[A, B]: (A x B) -> B

  def product[A, B, C](f: A -> B, g: A -> C): A -> (B x C)

  def term[A]: A -> I

  def left_unit[A]: (I x A) <-> A = <->(proj2[I, A], product(term, id))
  def right_unit[A]: (A x I) <-> A = <->(proj1[A, I], product(id, term))
  def assoc[A, B, C]: (A x (B x C)) <-> ((A x B) x C) =
    <->(
      product(product(proj1, compose(proj1[B, C], proj2)), compose(proj2[B, C], proj2)),
      product(compose(proj1[A, B], proj1), product(compose(proj2[A, B], proj1), proj2)))

  def swap[A, B]: (A x B) -> (B x A) = product(proj2, proj1)

  def tensor[A, B, C, D](f: A -> B, g: C -> D): (A x C) -> (B x D) =
    product(compose(f, proj1), compose(g, proj2))

  class CartesianLaws {
    def lcomp[A, B, C](f: A -> B, g: A -> C) =
      (product(f, g) >> proj1) === f

    def rcomp[A, B, C](f: A -> B, g: A -> C) =
      (product(f, g) >> proj2) === g
  }
}

trait CartesianClosed[->[_, _], x[_, _], ==>[_, _], I] extends Cartesian[->, x, I] with Closed[->, x, ==>, I]