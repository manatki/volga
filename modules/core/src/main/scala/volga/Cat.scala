package volga

import simulacrum.{op, typeclass}

@typeclass
trait Cat[->[_, _]] extends Identity[->] {
  def id[A]: A -> A

  @op("<<<", alias = true)
  def compose[A, B, C](f: B -> C, g: A -> B): A -> C

  @op(">>>", alias = true)
  def andThen[A, B, C](f: A -> B)(g: B -> C): A -> C =
    compose(g, f)

  implicit class CatOps[A, B](f: A -> B) {
    def o[C](g: C -> A): C -> B  = compose(f, g)
    def >>[C](g: B -> C): A -> C = compose(g, f)
  }
}
