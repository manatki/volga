package volga

import cats.Eq
import cats.syntax.order._
import monocle.PLens
import monocle.macros.{GenLens, Lenses, PLenses}
import monocle.syntax.all._
import monocle.function.all._

import scala.reflect.macros.blackbox

class syntaxMacro(val c: blackbox.Context) {
  import c.universe._

  val sym = reify(syntax)

  def sarr(body: c.Tree)(vb: Tree): c.Tree = arr(body)

  def arr(body: c.Tree): c.Tree = {
    val elems: List[Any] = body match {
      case q"(..$xs) => $b" =>
        b match {
          case q"{..$ls}" => ls.toList ::: xs.toList
          case _          => List(xs, b)

        }
      case _ => List(body)
    }
    val sss = elems.map(e => s"-----------\n$e\n").mkString
    q"""
       println($sss)
       null
      """
  }
}

object syntax {
  class V[X]

  private object varro extends V[Any]
  private def varr[X]: V[X] = varro.asInstanceOf[V[X]]

  object ----

  def arr[P[_, _]] = new MkArr[P]

  class MkArr[P[_, _]] {
    def apply[VB, B](body: () => VB)(implicit vb: Vars[B, VB]): P[Unit, B] = macro syntaxMacro.sarr

    def apply[A, VB, B](body: V[A] => VB)(implicit vb: Vars[B, VB]): P[A, B] = macro syntaxMacro.sarr

    def apply[A1, A2, VB, B](body: (V[A1], V[A2]) => VB)(implicit vb: Vars[B, VB]): P[(A1, A2), B] =
      macro syntaxMacro.sarr

    def apply[A1, A2, A3, VB, B](body: (V[A1], V[A2], V[A3]) => VB)(implicit vb: Vars[B, VB]): P[(A1, A2, A3), B] =
      macro syntaxMacro.sarr

    def apply[A1, A2, A3, A4, VB, B](body: (V[A1], V[A2], V[A3], V[A4]) => VB)(implicit vb: Vars[B, VB]): P[(A1, A2, A3), B] =
      macro syntaxMacro.sarr
  }

  implicit class ArrSyn[P[_, _], A, B](val s: P[A, B]) extends AnyVal {
    def apply[VA, VB](v: VA)(implicit va: Vars[A, VA], vb: Vars[B, VB]): VB = vb.va
  }

  final case class Vars[A, VA] private (va: VA)

  object Vars extends LowLevelVars {
    implicit val varIn0: Vars[Unit, Unit] = Vars(varr)

    implicit def varIn2[A, B]: Vars[(A, B), (V[A], V[B])]                         = Vars((varr, varr))
    implicit def varIn3[A, B, C]: Vars[(A, B, C), (V[A], V[B], V[C])]             = Vars((varr, varr, varr))
    implicit def varIn4[A, B, C, D]: Vars[(A, B, C, D), (V[A], V[B], V[C], V[D])] = Vars((varr, varr, varr, varr))
  }

  trait LowLevelVars {
    implicit def varIn1[A]: Vars[A, V[A]] = Vars(varr)
  }
}

final case class Assoc[K, I, O](key: K, in: I, out: O)
object Assoc {
  def out[K, I, O, O1] = PLens((_: Assoc[K, I, O]).out)((o1: O1) => _.copy(out = o1))
}

@Lenses
final case class Collect[K, M, I, O](singles: List[Assoc[K, List[I], O]],
                                     multis: Map[M, Assoc[K, List[I], Vector[Option[O]]]]) {
  import Assoc.out
  def addSingle(key: K, ins: List[I], out: O) =
    this &|-> Collect.singles modify (Assoc(key, ins, out) :: _)
  def startMultiIn(key: K, ins: List[I], m: M, arity: Int) =
    this &|-> Collect.multis ^|-> at(m) set Some(Assoc(key, ins, Vector.fill[Option[O]](arity)(None)))
  def addMultiOut(m: M, idx: Int, out: O) =
    this &|-> Collect.multis ^|-? index(m) ^|-> Assoc.out ^|-? index(idx) set Some(out)

  def assocs: List[Assoc[K, List[I], List[O]]] =
    (singles.iterator.map(out.modify(List(_))) ++
      multis.values.iterator.map(out.modify(_.toList.flatten))).toList
}
