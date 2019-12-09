package volga

trait SemCatLike[->[_, _], x[_, _]]

trait SemigropalCat[->[_, _], x[_, _]] extends Cat[->] with SemCatLike[->, x] {
  type Tensor[a, b] = a x b

  def assocl[A, B, C]: (A x (B x C)) -> ((A x B) x C)
  def assocr[A, B, C]: ((A x B) x C) -> (A x (B x C))

  def tensor[A, B, C, D](f: A -> B, g: C -> D): (A x C) -> (B x D)

  def split[A, B, C, D](f: A -> B, g: C -> D): (A x C) -> (B x D) = tensor(f, g)
}

trait MonCatLike[->[_, _], x[_, _], I] extends SemCatLike[->, x]

trait MonoidalCat[->[_, _], x[_, _], I] extends SemigropalCat[->, x] with MonCatLike[->, x, I] {
  type One = I

  def lunit[A]: (I x A) -> A
  def unitl[A]: A -> (I x A)
  def runit[A]: (A x I) -> A
  def unitr[A]: A -> (A x I)
}

trait Sym[->[_, _], x[_, _]] extends SemigropalCat[->, x] {
  def swap[A, B]: (A x B) -> (B x A)

  def assocr[A, B, C]: ((A x B) x C) -> (A x (B x C)) =
    andThen(swap[A x B, C])(
      andThen(tensor(id[C], swap[A, B]))(
        andThen(assocl[C, B, A])(
          andThen(tensor(swap[C, B], id[A]))(swap[B x C, A])
        )
      )
    )
}

trait Symon[->[_, _], x[_, _], I] extends Sym[->, x] with MonoidalCat[->, x, I] {
  def runit[A]: (A x I) -> A = compose(lunit[A], swap)
  def unitr[A]: A -> (A x I) = andThen(unitl[A])(swap)
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
    lcurry(compose(f, lunit))

  def precmp[A, B, C](f: A -> B): (C ==> A) -> (C ==> B) =
    lcurry(compose(f, lapply[C, A]))

  def postcmp[A, B, C](f: A -> B): (B ==> C) -> (A ==> C) =
    lcurry(compose(lapply[B, C], tensor(id[B ==> C], f)))

  //bifunctoriality of closure
  def promap[A, B, C, D](f: A -> B, g: C -> D): (D ==> A) -> (C ==> B) =
    compose(precmp[A, B, C](f), postcmp[C, D, A](g))
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

  def choose[A, B, C, D](f: A -> C, g: B -> D): (A :+ B) -> (C :+ D) =
    sum(andThen(f)(inj1), andThen(g)(inj2))

  def init[A]: O -> A

  def distribr[A, B, C]: ((A x B) :+ (A x C)) -> (A x (B :+ C)) = sum(tensor(id, inj1), tensor(id, inj2))
}

trait DistributiveCat[->[_, _], x[_, _], I, :+[_, _], O] extends Bicartesian[->, x, I, :+, O] {
  def distribl[A, B, C]: (A x (B :+ C)) -> ((A x B) :+ (A x C))
}

trait CartesianClosed[->[_, _], x[_, _], ==>[_, _], I] extends Cartesian[->, x, I] with Closed[->, x, ==>, I]

trait BicartesianClosed[->[_, _], x[_, _], ==>[_, _], I, :+[_, _], O]
    extends DistributiveCat[->, x, I, :+, O] with CartesianClosed[->, x, ==>, I] {
  def distribl[A, B, C]: (A x (B :+ C)) -> ((A x B) :+ (A x C)) =
    runcurry[A, (B :+ C), (A x B) :+ (A x C)](
      sum[B, C, A ==> ((A x B) :+ (A x C))](
        andThen(lunapply[B, A])(precmp(andThen(swap[B, A])(inj1[A x B, A x C]))),
        andThen(lunapply[C, A])(precmp(andThen(swap[C, A])(inj2[A x B, A x C])))
      )
    )
}
