package volga.solve
import cats.data.NonEmptyList

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable
import cats.syntax.either._
import cats.syntax.applicative._
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.parallel._
import cats.instances.either._
import cats.instances.parallel._

object Permutations {
  def cycles(xs: Vector[Int]): Vector[Vector[Int]] = {
    val seen = mutable.BitSet()
    val bldr = Vector.newBuilder[Vector[Int]]
    for (start <- xs.indices if !seen(start)) {
      val subldr = Vector.newBuilder[Int]
      var i      = start
      do {
        subldr += i
        seen += i
        i = xs(i)
      } while (i != start)
      bldr += subldr.result()
    }
    bldr.result()
  }

  def cycleSwaps(xs: Vector[Int]): Vector[(Int, Int)] = xs match {
    case head +: rest => rest map head.->
    case _            => Vector()
  }

  def swaps(xs: Vector[Int]): Vector[(Int, Int)] = cycles(xs).flatMap(cycleSwaps)

  def buildPerm[A](xs: Vector[A], ys: Vector[A]): Either[NonEmptyList[String], Vector[Int]] = {
    val xset = xs.toSet
    val yset = ys.toSet

    s"xs has non unique elements $xs".leftNel.whenA(xset.size != xs.size) &>
      "ys has non unique elements".leftNel.whenA(yset.size != ys.size) &>
      s"xs and ys has different sizes: ${xs.size}, ${ys.size}".leftNel.whenA(xs.size != ys.size) &>
      s"xs and ys contain different elements : ${xset -- yset} vs ${yset -- xset}".leftNel.whenA(xset != yset) map
      (_ => xs.map(ys.iterator.zipWithIndex.toMap))
  }
}
