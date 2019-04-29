package volga

import cats.data.Kleisli
import syntax._
import cats.syntax.compose._
import cats.syntax.arrow._

object Chepukku {
  type LK[A, B] = Kleisli[List, A, B]

  val chars: LK[String, Int] = Kleisli(s => s.map(_.toInt).toList)
  def rep(s: String): LK[Int, String] = Kleisli(i => List.fill(i)(s))

  def tututu: LK[(Int, String), (String, Int)] =
    arr[LK] { (x: V[Int], y: V[String]) =>
      val a = chars(y)
      val b = rep("a")(x)
      ----
      val c = rep("b")(a)
      val d = chars(b)
      ----
      (c, d)
    }

  def main(args: Array[String]): Unit = {
    tututu
  }
}
