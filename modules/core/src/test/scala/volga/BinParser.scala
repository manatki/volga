package volga
import volga.solve.{Bin, BinZipper}
import fastparse._
import MultiLineWhitespace._
import volga.solve.Bin.{Branch, Leaf}
import cats.syntax.flatMap._
import cats.syntax.show._
import cats.instances.option._
import cats.instances.string._
import cats.instances.either._

object BinParser extends App {
  def leaf[_: P]                = P(CharPred(_.isLetterOrDigit).rep.!).map(Leaf(_))
  def branch[_: P]              = P("(" ~ bin ~ "," ~ bin ~ ")").map((Branch[String] _).tupled)
  def bin[_: P]: P[Bin[String]] = branch | leaf
  def binTree[_: P]             = bin ~ End

  val tree  = parse("((1, (2, 3)), (4, (5, 6)))", binTree(_), true).get.value
  val tree2 = parse("(3, (((4, 1), 6), (5, 2))) ", binTree(_)).get.value

  val ta = tree.adaptation(tree2).right.get
  println(ta)

  println(tree.modAll(ta).show)
  val linearized = tree.zipper.linearize.top
  println(linearized.tree.show)
  val s12 = linearized.walk(_.rotateL, _.goLeft, _.swap, _.goUp, _.rotateR)
  val s13 = linearized.swapElems(1, 5)
  println(s12.map(_.tree).show)
  println(s13.map(_.tree).show)
  val hist = linearized.history
  println(hist, tree.modAll(hist).show)

  println(parse("(1, (2, (3, (4, (5, 6)))))", binTree(_)).get.value.modAll(hist.invertAll).show)
}
