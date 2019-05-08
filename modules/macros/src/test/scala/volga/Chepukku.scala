package volga

import cats.arrow.Arrow
import cats.data.Kleisli
import syntax._
import cats.instances.list._
import cats.syntax.compose._
import cats.syntax.arrow._


import scala.Function.tupled

object Chepukku {
  type LK[A, B] = Kleisli[List, A, B]

  val chars: LK[String, Int]           = Kleisli(s => s.map(_.toInt).toList)
  val charIdxs: LK[String, (Int, Int)] = Kleisli(s => s.map(_.toInt).zipWithIndex.toList)
  def rep(s: String): LK[Int, String]  = Kleisli(i => List.fill(i)(s))

  def tututu: LK[(Int, String), (String, Int)] =
    arr[LK] { (x: V[Int], y: V[String]) =>
      val (a, i) = charIdxs(y)
      val b      = rep("a")(x)
//      ----
      val c      = rep("b")(a)
      val (d, j) = charIdxs(b)
//      ----
      val u  = Arrow[LK].id[Int](i)
      val z  = Arrow[LK].lift(tupled[Int, Int, Int, Int](_ + _ + _))(d, i, j)
      val z1 = Arrow[LK].lift(tupled[Int, Int, Int, Int](_ + _ + _))(d, u, j)

      (c, z)
    }

  def main(args: Array[String]): Unit = {
    println(tututu.run((2, "kek")))
  }
}
