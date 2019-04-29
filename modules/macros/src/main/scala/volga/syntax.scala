package volga

import scala.reflect.macros.blackbox

class syntaxMacro(val c: blackbox.Context) {
  import c.universe._
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
  }

  implicit class ArrSyn[P[_, _], A, B](val s: P[A, B]) extends AnyVal {
    def apply[VA, VB](v: VA)(implicit va: Vars[A, VA], vb: Vars[B, VB]): VB = vb.va
  }

  final case class Vars[A, VA] private (va: VA)

  object Vars {
    implicit val varIn0: Vars[Unit, Unit]                             = Vars(varr)
    implicit def varIn1[A]: Vars[A, V[A]]                             = Vars(varr)
    implicit def varIn2[A, B]: Vars[(A, B), (V[A], V[B])]             = Vars((varr, varr))
    implicit def varIn3[A, B, C]: Vars[(A, B, C), (V[A], V[B], V[C])] = Vars((varr, varr, varr))
  }
}
