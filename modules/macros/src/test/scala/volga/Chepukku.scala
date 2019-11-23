package volga

import cats.arrow.Arrow
import cats.data.Kleisli
import syntax._
import cats.instances.list._
import cats.syntax.compose._
import cats.syntax.arrow._

import scala.Function.tupled

object Chepukku {
  trait LK[A, B] {
    def run(a: A): List[B]
  }

  object LK {
    def lift[A, B](f: A => B): LK[A, B] = a => List(f(a))

    implicit val arr: Arrow[LK] = new Arrow[LK] {
      def lift[A, B](f: A => B): LK[A, B]                      = a => List(f(a))
      def first[A, B, C](fa: LK[A, B]): LK[(A, C), (B, C)]     = { case (a, c) => fa.run(a).map(b => (b, c)) }
      def compose[A, B, C](f: LK[B, C], g: LK[A, B]): LK[A, C] = a => g.run(a).flatMap(f.run)
    }
  }

  val chars: LK[String, Int]           = s => s.map(_.toInt).toList
  val charIdxs: LK[String, (Int, Int)] = s => s.map(_.toInt).zipWithIndex.toList
  def rep(s: String): LK[Int, String]  = i => List.fill(i)(s)

  def tututu: LK[(Int, String), (String, Int)] =
    arr[LK] { (x: V[Int], y: V[String]) =>
      val (a, i) = charIdxs(y)
      val b      = rep("a")(x)
//      ----
      val c      = rep("b")(a)
      val (d, j) = charIdxs(b)
//      ----
      val u  = Arrow[LK].id[Int](i)
      val z  = LK.lift(tupled[Int, Int, Int, Int](_ + _ + _))(d, i, j)
      val z1 = LK.lift(tupled[Int, Int, Int, Int](_ + _ + _))(d, u, j)

      (c, z)
    }

  def main(args: Array[String]): Unit = {
    println(tututu.run((2, "kek")))
  }
}
