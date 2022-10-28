package volga

import cats.arrow.Category
import simulacrum.{op, typeclass}
trait Identity[->[_, _]] {
  def id[A]: A -> A
}

object Identity extends CatInstanceChain[Identity]

trait Cat[->[_, _]] extends Identity[->] {
  def id[A]: A -> A

  def compose[A, B, C](f: B -> C, g: A -> B): A -> C

  def andThen[A, B, C](f: A -> B)(g: B -> C): A -> C = compose(g, f)
}

object Cat extends CatInstanceChain[Cat]

trait CatInstanceChain[TC[a[_, _]] >: Cat[a]] {
  implicit def catFromCats[->[_, _]](implicit cat: Category[->]): TC[->] =
    new Cat[->] {
      def id[A]: A -> A                                  = cat.id
      def compose[A, B, C](f: B -> C, g: A -> B): A -> C = cat.compose(f, g)
    }
}
