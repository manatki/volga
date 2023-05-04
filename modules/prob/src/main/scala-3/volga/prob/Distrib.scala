package volga.prob

import Numeric.Implicits.given
import scala.collection.mutable

def collectKeys[A, B, C](xs: Map[A, B])(f: PartialFunction[A, C]): Map[C, B] = 
    xs.collect{ case (f(c) , b) => (c, b) }

enum Distrib[P, A]:
  case Table(table: Map[A, P])
  case Zip[P, A, B](ad: Distrib[P, A], bd: Distrib[P, B])             extends Distrib[P, (A, B)]
  case PartLift[A, B, P](ad: Distrib[P, A], f: PartialFunction[A, B]) extends Distrib[P, B]

  def iterator(using Numeric[P]): Iterator[(A, P)] = this match
    case Zip(ad, bd) =>
      for
        (a, pa) <- ad.iterator
        (b, pb) <- bd.iterator
      yield ((a, b), pa * pb)

    case Table(table) => table.iterator

    case PartLift(ad, f) => ad.iterator.collect{ case (f(b), p) => (b, p) }

  def barrier(using Numeric[P]): Distrib[P, A] = 
    val m = mutable.Map.empty[A, P]
    val zero = Numeric[P].zero
    for (i, p) <- iterator do
        m(i) = m.getOrElse(i, zero) + p

    Table(m.toMap)
