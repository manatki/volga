package volga
import volga.solve.{Bin, BinZipper}
import fastparse._
import MultiLineWhitespace._
import volga.solve.Bin.{Branch, Bud, Leaf}
import cats.syntax.flatMap._
import cats.syntax.show._
import cats.instances.option._
import cats.instances.string._
import cats.instances.either._
import volga.solve.BinOp.{L, R}

object BinParser extends App {
  def leaf[_: P]: P[Bin[String]] = P(CharPred(_.isLetterOrDigit).rep(1).!).map(Leaf(_))
  def bud[_: P]                  = P("*").map(_ => Bud)
  def branch[_: P]               = P("(" ~ bin ~ "," ~ bin ~ ")").map((Branch[String] _).tupled)
  def bin[_: P]: P[Bin[String]]  = branch | leaf | bud
  def binTree[_: P]              = bin ~ End

  val tree  = parse("((1,  (* , (2, 3))), (4, (5, 6)))", binTree(_), true).get.value
  val tree2 = parse("(3, (((4, 1), (*, 6)), (5, (*, 2)))) ", binTree(_)).get.value

  val ta = tree.adaptation(tree2).fold(s => throw new Exception(s), identity)
  println(ta)
  val tc = tree.zipper.clean.tree
  println(tc.show)

  println(tree.modAll(ta).show)

  val linearized = tree.zipper.linearize.top
  println(linearized.tree.show)
  val s12 = linearized.walk(_.rotate(L), _.goLeft, _.swap, _.goUp, _.rotate(R))
  val s13 = linearized.swapElems(1, 5)
  println(s12.map(_.tree).show)
  println(s13.map(_.tree).show)
  val hist = linearized.history
  println(hist, tree.modAll(hist).show)

  println(parse("(1, (2, (3, (4, (5, 6)))))", binTree(_)).get.value.modAll(hist.invertAll).show)
}
