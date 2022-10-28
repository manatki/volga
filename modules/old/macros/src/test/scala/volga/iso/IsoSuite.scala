package volga.iso

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import volga.data.Iso
import volga.syntax.comp._
import volga.syntax.cat._
import volga.syntax.symmon._

class IsoSuite extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  val iso = symon[Iso, (*, *), Unit]

  val m32                   = (1L << 32) - 1
  def intBits(x: Int): Long = ((1L << 32) + x) & m32

  val longInts = Iso[(Int, Int), Long] {
    case (a, b) => (intBits(a) << 32) | intBits(b)
  }(l => ((l >> 32).toInt, (l & m32).toInt))

  def plus(x: Int) = Iso[Int, Int](_ + x)(_ - x)

  def intString = Iso[String, Int](_.toInt)(_.toString)
  def splitStr = Iso[String, (String, String)](
    _.split(",", 2) match { case Array(x, y) => (x, y) }
  ) { case (s1, s2) => s"$s1,$s2" }

  def iso1 = iso { (x: V[Int], y: V[Int]) =>
    longInts(x, y)
  }

  def add1Both = iso { (x: V[String]) =>
    val (sa, sb) = splitStr(x)
    val a        = intString(sa)
    val b        = intString(sb)
    val a1       = plus(1)(a)
    val b1       = plus(1)(b)
    val sa1      = intString.invert(a1)
    val sb1      = intString.invert(b1)
    splitStr.invert(sa1, sb1)
  }

  "add1Both" should "work forwards" in forAll { (x: Int, y: Int) =>
    add1Both.to(s"$x,$y") must ===(s"${x + 1},${y + 1}")
  }
  "add1Both" should "work backwards" in forAll { (x: Int, y: Int) =>
    add1Both.from(s"$x,$y") must ===(s"${x - 1},${y - 1}")
  }
}
