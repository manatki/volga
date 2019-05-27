package volga

import cats.arrow.Category

trait Cat[->[_, _]] {
  def id[A]: A -> A
  def compose[A, B, C](f: B -> C, g: A -> B): A -> C
  def andThen[A, B, C](f: A -> B, g: B -> C): A -> C =
    compose(g, f)

  implicit class CatOps[A, B](f: A -> B) {
    def o[C](g: C -> A): C -> B  = compose(f, g)
    def >>[C](g: B -> C): A -> C = compose(g, f)
  }
}

trait SemigropalCat[->[_, _], x[_, _]] extends Cat[->] {
  def assocl[A, B, C]: (A x (B x C)) -> ((A x B) x C)
  def assocr[A, B, C]: ((A x B) x C) -> (A x (B x C))

  def tensor[A, B, C, D](f: A -> B, g: C -> D): (A x C) -> (B x D)

  implicit class SemiCatOps[A, B](f: A -> B) {
    def x[C, D](g: C -> D): (A x C) -> (B x D) = tensor(f, g)
  }
}

trait MonoidalCat[->[_, _], x[_, _], I] extends SemigropalCat[->, x] {
  def lunit[A]: (I x A) -> A
  def unitl[A]: A -> (I x A)
  def runit[A]: (A x I) -> A
  def unitr[A]: A -> (A x I)
}

trait Sym[->[_, _], x[_, _]] extends SemigropalCat[->, x] {
  def swap[A, B]: (A x B) -> (B x A)

  def assocr[A, B, C]: ((A x B) x C) -> (A x (B x C)) =
    swap[B x C, A] o
      (swap[C, B] x id[A]) o
      assocl o
      (id[C] x swap[A, B]) o
      swap
}

trait Symon[->[_, _], x[_, _], I] extends Sym[->, x] with MonoidalCat[->, x, I] {
  def runit[A]: (A x I) -> A = lunit[A] o swap
  def unitr[A]: A -> (A x I) = unitl[A] >> swap
}

trait SemiClosed[->[_, _], x[_, _], ==>[_, _]] extends Sym[->, x] {
  def lcurry[A, B, C](p: (A x B) -> C): A -> (B ==> C)
  def luncurry[A, B, C](p: A -> (B ==> C)): (A x B) -> C

  def rcurry[A, B, C](p: (A x B) -> C): B -> (A ==> C) =
    lcurry(compose(p, swap))
  def runcurry[A, B, C](p: B -> (A ==> C)): (A x B) -> C =
    compose(luncurry(p), swap)

  def lapply[A, B]: ((A ==> B) x A) -> B = luncurry(id)
  def rapply[A, B]: (A x (A ==> B)) -> B = runcurry(id)

  def lunapply[A, B]: A -> (B ==> (A x B)) = lcurry(id)
  def runapply[A, B]: B -> (A ==> (A x B)) = rcurry(id)
}

trait Closed[->[_, _], x[_, _], ==>[_, _], I] extends SemiClosed[->, x, ==>] with MonoidalCat[->, x, I] {
  def ident[A]: I -> (A ==> A)  = lcurry(lunit)
  def choose[A]: A -> (I ==> A) = lcurry(runit)
  def unchoose[A]: (I ==> A) -> A =
    compose(lapply[I, A], unitr[I ==> A])

  def abstraction[A, B](f: A -> B): I -> (A ==> B) =
    lcurry(f o lunit)

  def precmp[A, B, C](f: A -> B): (C ==> A) -> (C ==> B) =
    lcurry(f o lapply[C, A])

  def postcmp[A, B, C](f: A -> B): (B ==> C) -> (A ==> C) =
    lcurry(lapply[B, C] o tensor(id[B ==> C], f))

  //bifunctoriality of closure
  def promap[A, B, C, D](f: A -> B, g: C -> D): (D ==> A) -> (C ==> B) =
    precmp[A, B, C](f) o postcmp[C, D, A](g)
}

trait Cartesian[->[_, _], x[_, _], I] extends Symon[->, x, I] {
  def proj1[A, B]: (A x B) -> A
  def proj2[A, B]: (A x B) -> B

  def product[A, B, C](f: A -> B, g: A -> C): A -> (B x C)

  def term[A]: A -> I

  override def lunit[A]: (I x A) -> A = proj2
  override def unitl[A]: A -> (I x A) = product(term, id)
  override def runit[A]: (A x I) -> A = proj1
  override def unitr[A]: A -> x[A, I] = product(id, term)
  override def swap[A, B]: (A x B) -> (B x A) =
    product(proj2, proj1)
  override def assocl[A, B, C]: (A x (B x C)) -> ((A x B) x C) =
    product(product(proj1, compose(proj1[B, C], proj2)), compose(proj2[B, C], proj2))
  override def assocr[A, B, C]: ((A x B) x C) -> (A x (B x C)) =
    product(compose(proj1[A, B], proj1), product(compose(proj2[A, B], proj1), proj2))
  override def tensor[A, B, C, D](f: A -> B, g: C -> D): (A x C) -> (B x D) =
    product(compose(f, proj1), compose(g, proj2))
}

trait Bicartesian[->[_, _], x[_, _], I, :+[_, _], O] extends Cartesian[->, x, I] {
  def inj1[A, B]: A -> (A :+ B)
  def inj2[A, B]: B -> (A :+ B)

  def sum[A, B, C](f: A -> C, g: B -> C): (A :+ B) -> C

  def choose[A, B, C, D](f: A -> C, g: B -> D): (A :+ B) -> (C :+ D) = sum(f >> inj1, g >> inj2)

  def init[A]: O -> A

  def distribr[A, B, C]: ((A x B) :+ (A x C)) -> (A x (B :+ C)) = sum(id x inj1, id x inj2)

  implicit class BicatOps[A, B](f: A -> B) {
    def +[C, D](g: C -> D): (A :+ C) -> (B :+ D) = choose(f, g)
  }

}

trait CartesianClosed[->[_, _], x[_, _], ==>[_, _], I] extends Cartesian[->, x, I] with Closed[->, x, ==>, I]

trait BicartesianClosed[->[_, _], x[_, _], ==>[_, _], I, :+[_, _], O]
    extends Bicartesian[->, x, I, :+, O] with CartesianClosed[->, x, ==>, I] {
  def distribl[A, B, C]: (A x (B :+ C)) -> ((A x B) :+ (A x C)) =
    runcurry(
      sum(
        lunapply >> precmp(swap >> inj1),
        lunapply >> precmp(swap >> inj2)
      )
    )
}
