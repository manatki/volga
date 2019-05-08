package volga

import cats.arrow.Category

trait Monoidal[-->[_, _], x[_, _], i] extends Category[-->] {
  def lunit[a]: (i x a) --> a
  def unitl[a]: a --> (i x a)
  def runit[a]: (a x i) --> a
  def unitr[a]: a --> (a x i)

  def assocl[a, b, c]: (a x (b x c)) --> (a x b x c)
  def assocr[a, b, c]: (a x b x c) --> (a x (b x c))
}

trait Symon[-->[_, _], x[_, _], i] extends Monoidal[-->, x, i]{
  def swap[a, b]: (a x b) --> (b x a)
}
