package volga

import volga.syntax.comp._
import volga.syntax.all._

final abstract class Combo[+A, +B]
case object Dummy
sealed trait Trace[-A, +B]

case class Named(name: String)                                            extends Trace[Any, Nothing]
case class Split[-A, +B, -C, +D](first: Trace[A, B], second: Trace[C, D]) extends Trace[Combo[A, C], Combo[B, D]]
case class Continue[-A, B, +C](begin: Trace[A, B], continue: Trace[B, C]) extends Trace[A, C]

object Trace {
  implicit object symon extends Symon[Trace, Combo, Dummy.type] {
    def swap[A, B]: Trace[Combo[A, B], Combo[B, A]] = Named("swap")

    def lunit[A]: Trace[Combo[Dummy.type, A], A] = Named("eat")

    def unitl[A]: Trace[A, Combo[Dummy.type, A]] = Named("spawn")

    def assocl[A, B, C]: Trace[Combo[A, Combo[B, C]], Combo[Combo[A, B], C]] = Named("assoc")

    def tensor[A, B, C, D](f: Trace[A, B], g: Trace[C, D]): Trace[Combo[A, C], Combo[B, D]] = Split(f, g)

    def id[A]: Trace[A, A] = Named("identity")

    def compose[A, B, C](f: Trace[B, C], g: Trace[A, B]): Trace[A, C] = Continue(g, f)
  }
}

object Nekote {
  def main(args: Array[String]) =  {
    val intToString: Trace[Int, String]              = Named("intToString")
    val stringToDouble: Trace[String, Double]        = Named("stringToDouble")
    val concat: Trace[String Combo String, String]   = Named("concat")
    val separate: Trace[String, String Combo String] = Named("separate")

    val trace = symon[Trace, Combo, Dummy.type]

    def test1: Trace[Combo[Int, Int], Double] = trace { (a: V[Int], b: V[Int]) =>
      val x = intToString(a)
      val y = intToString(b)
      val r = concat(x, y)

      stringToDouble(r)
    }
    //  println(test1)

    def test2: Trace[Combo[Int, Int], Double] = trace { (a: V[Int], b: V[Int]) =>
      val x = intToString(a)
      ----
      val y: V[String] = intToString(b)
      val r            = concat(x, y)

      stringToDouble(r)
    }

    //  println(test2)

    def test3 = trace { (a: V[Int], b: V[Int], c: V[Int]) =>
      val x1 = intToString(c)
      val x2 = intToString(a)
      val x3 = intToString(b)
      val x4 = concat(x1, x2)
      val x5 = concat(x4, x3)

      stringToDouble(x5)
    }

    //  println(test3)

    def test4 = trace { (a: V[String], b: V[String]) =>
      val (x1, x2) = separate(a)
      val (x3, x4) = separate(b)

      val y1 = concat(x1, x3)
      val y2 = concat(x2, x4)

      concat(y1, y2)
    }

    println(test4)
  }
}
