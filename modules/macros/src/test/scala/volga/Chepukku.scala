package volga

import cats.arrow.Arrow
import cats.data.Kleisli
import syntax.comp._
import cats.instances.list._
import cats.syntax.compose._
import syntax.cat._
import syntax.arr._

import scala.Function.tupled

object Chepukku {
  trait LK[A, B] {
    def run(a: A): List[B]
  }
  object LK {
    def lift[A, B](f: A => B): LK[A, B] = a => List(f(a))

    implicit val arr: Arr[LK] = new Arr[LK] {
      def lift[A, B](f: A => B): LK[A, B]                           = a => List(f(a))

      def split[A, B, C, D](f: LK[A, C], g: LK[B, D]): LK[(A, B), (C, D)] = {
        case (a, c) => for (b <- f.run(a); d <- g.run(c)) yield (b, d)
      }

      def compose[A, B, C](f: LK[B, C], g: LK[A, B]): LK[A, C] = a => g.run(a).flatMap(f.run)
    }
  }

  val chars: LK[String, Int]           = s => s.map(_.toInt).toList
  val charIdxs: LK[String, (Int, Int)] = s => s.map(_.toInt).zipWithIndex.toList
  def rep(s: String): LK[Int, String]  = i => List.fill(i)(s)

  val listKleisli = arr[LK]

  def tututu: LK[(Int, String), (String, Int)] =
    listKleisli { (x: V[Int], y: V[String]) =>
      val (a, i) = charIdxs(y)
      val b      = rep("a")(x)
//      ----
      val c      = rep("b")(a)
      val (d, j) = charIdxs(b)
//      ----
      val u  = Arr[LK].id[Int](i)
      val z  = LK.lift(tupled[Int, Int, Int, Int](_ + _ + _))(d, i, j)
      val z1 = LK.lift(tupled[Int, Int, Int, Int](_ + _ + _))(d, u, j)

      (c, z)
    }

  def main(args: Array[String]): Unit = {
    println(tututu.run((2, "kek")))
  }
}
