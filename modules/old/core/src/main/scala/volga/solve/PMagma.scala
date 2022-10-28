package volga.solve

import simulacrum.{op, typeclass}

@typeclass
trait PMagma[A] {
  def empty: A

  @op("##")
  def combine(x: A, y: A): A
}
