package volga

import cats.arrow.Category

trait SemigropalCat[-->[_, _], x[_, _]] extends Category[-->] {
  def assocl[a, b, c]: (a x (b x c)) --> ((a x b) x c)
  def assocr[a, b, c]: ((a x b) x c) --> (a x (b x c))
}

trait MonoidalCat[-->[_, _], x[_, _], i] extends SemigropalCat[-->, x] {
  def lunit[a]: (i x a) --> a
  def unitl[a]: a --> (i x a)
  def runit[a]: (a x i) --> a
  def unitr[a]: a --> (a x i)
}

trait Sym[-->[_, _], x[_, _]] extends SemigropalCat[-->, x] {
  def swap[a, b]: (a x b) --> (b x a)
}

trait Symon[-->[_, _], x[_, _], i] extends Sym[-->, x] with MonoidalCat[-->, x, i]

trait SemiClosed[-->[_, _], x[_, _], ==>[_, _]] extends Sym[-->, x] {
  def lcurry[a, b, c](p: (a x b) --> c): a --> (b ==> c)
  def luncurry[a, b, c](p: a --> (b ==> c)): (a x b) --> c

  def rcurry[a, b, c](p: (a x b) --> c): b --> (a ==> c)   = lcurry(compose(p, swap))
  def runcurry[a, b, c](p: b --> (a ==> c)): (a x b) --> c = compose(luncurry(p), swap)

  def lapply[a, b]: ((a ==> b) x a) --> b = luncurry(id)
  def rapply[a, b]: (a x (a ==> b)) --> b = runcurry(id)

  def lunapply[a, b]: a --> (b ==> (a x b)) = lcurry(id)
  def runapply[a, b]: b --> (a ==> (a x b)) = rcurry(id)
}

trait Closed[-->[_, _], x[_, _], ==>[_, _], i] extends SemiClosed[-->, x, ==>] with MonoidalCat[-->, x, i] {
  def ident[a]: i --> (a ==> a)    = lcurry(lunit)
  def choose[a]: a --> (i ==> a)   = lcurry(runit)
  def unchoose[a]: (i ==> a) --> a = compose(lapply[i, a], unitr[i ==> a])
//  def cmp[a, b, c]: (a ==> b) --> ((c ==> a) ==> (c ==> b)) = ???
}
