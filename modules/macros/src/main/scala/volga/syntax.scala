package volga

import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.traverse._
import monocle.PLens
import monocle.function.all._
import monocle.macros.Lenses
import monocle.syntax.all._

import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

class syntaxMacro(val c: blackbox.Context) {
  import c.universe._

  object Syntax {
    val syn                          = reify(syntax).tree.symbol
    def unapply(tree: Tree): Boolean = tree.symbol == syn
  }

  object Split {
    def unapply(tree: Tree): Boolean =
      tree match {
        case q"${Syntax()}.----" => true
        case _                   => false
      }
  }

  object Names {
    def unapply(t: List[Tree]): Option[List[TermName]] =
      t.traverse { case q"${a: TermName}" => Some(a); case _ => None }
  }

  object ArrSyn {
    def unapply(tree: Tree): Option[(Tree, List[TermName])] =
      tree match {
        case q"${Syntax()}.ArrSyn[..$_]($smth).apply[..$_](..${Names(names)})(..$_)" => Some((smth, names))
        case q"$tree: $_"                                                            => unapply(tree)
        case _                                                                       => None
      }
  }

  object VarElem {
    val sub = "_(\\d+)".r
    def unapply(tree: Tree): Option[(TermName, Int)] =
      tree match {
        case q"${n: TermName}.${TermName(sub(i))}" =>
          try Some((n, i.toInt))
          catch { case NonFatal(_) => None }
        case _ => None
      }
  }

  def sarr(body: c.Tree)(vb: Tree): c.Tree = arr(body)

  def arr(body: c.Tree): c.Tree = {
    val elems: List[Any] = body match {
      case q"(..$xs) => $b" =>
        b match {
          case q"{..$ls}" =>
            val lm  = ls.toList.map(matchElem)
            val xns = xs.collect { case ValDef(_, name, _, _) => name }.toList
            val ps  = parse.collectBody(ls.toList, xns, matchElem)
            val ttt = ps.fold({ case (to, x) => c.abort(to.fold(c.enclosingPosition)(_.pos), x) }, identity)
            ttt :: lm.toList ::: xs.toList ::: xns
          case _ => List(xs, b)
        }
      case _ => List(body)
    }
    val sss = elems.map(e => s"-----------\n$e\n").mkString
    q"""
       println($sss)
       null
      """
  }

  def matchElem[A](t: Tree): ParseElem[Tree, TermName] = t match {
    case ValDef(_, name, _, ArrSyn(smth, args)) => ParseElem.Single(args, name, smth)
    case ValDef(_, name, _, VarElem(v, i))      => ParseElem.MultiAdd(v, name, i - 1)
    case ValDef(mods, name, t, Match(ArrSyn(smth, args), _)) if mods.hasFlag(Flag.SYNTHETIC) =>
      val arity = t.tpe match {
        case TypeRef(_, _, xs) => xs.length
        case _                 => 0
      }
      ParseElem.MultiStart(args, name, smth, arity)
    case q"${Split()}"          => ParseElem.Split
    case q"(..${Names(names)})" => ParseElem.Result(names)
    case l                      => ParseElem.Other(l)
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
    def apply[X, VB](v: V[X])(implicit vb: Vars[B, VB]): VB                                                 = vb.va
    def apply[X1, X2, VB](v1: V[X1], v2: V[X2])(implicit vb: Vars[B, VB]): VB                               = vb.va
    def apply[X1, X2, X3, VB](v1: V[X1], v2: V[X2], v3: V[X3])(implicit vb: Vars[B, VB]): VB                = vb.va
    def apply[X1, X2, X3, X4, VB](v1: V[X1], v2: V[X2], v3: V[X3], v4: V[X4])(implicit vb: Vars[B, VB]): VB = vb.va
  }

  final case class Vars[A, VA] private (va: VA)

  object Vars extends LowLevelVars {
    implicit val varIn0: Vars[Unit, Unit] = Vars(varr)

    implicit def varIn2[A, B]: Vars[(A, B), (V[A], V[B])]                         = Vars((varr, varr))
    implicit def varIn3[A, B, C]: Vars[(A, B, C), (V[A], V[B], V[C])]             = Vars((varr, varr, varr))
    implicit def varIn4[A, B, C, D]: Vars[(A, B, C, D), (V[A], V[B], V[C], V[D])] = Vars((varr, varr, varr, varr))
    implicit def varIn5[A, B, C, D, E]: Vars[(A, B, C, D, E), (V[A], V[B], V[C], V[D], V[E])] =
      Vars((varr, varr, varr, varr, varr))
  }

  trait LowLevelVars {
    implicit def varIn1[A]: Vars[A, V[A]] = Vars(varr)
  }
}

final case class Assoc[A, I, O](app: A, in: I, out: O)
object Assoc {
  def out[K, I, O, O1] = PLens((_: Assoc[K, I, O]).out)((o1: O1) => _.copy(out = o1))
}

@Lenses
final case class Collect[A, M, I, O](singles: List[Assoc[A, List[I], O]] = Nil,
                                     multis: Map[M, Assoc[A, List[I], Vector[Option[O]]]] = Map.empty[M, Nothing]) {
  import Assoc.out
  def addSingle(app: A, ins: List[I], out: O) =
    this &|-> Collect.singles modify (Assoc(app, ins, out) :: _)
  def startMultiIn(app: A, ins: List[I], m: M, arity: Int) =
    this &|-> Collect.multis ^|-> at(m) set Some(Assoc(app, ins, Vector.fill[Option[O]](arity)(None)))
  def addMultiOut(m: M, idx: Int, out: O) =
    this &|-> Collect.multis ^|-? index(m) ^|-> Assoc.out ^|-? index(idx) set Some(out)

  def assocs: List[Assoc[A, List[I], List[O]]] =
    (singles.iterator.map(out.modify(List(_))) ++
      multis.values.iterator.map(out.modify(_.toList.flatten))).toList

  def isEmpty = singles.isEmpty && multis.isEmpty
}

sealed trait ParseElem[+T, +N]

object ParseElem {
  final case class Single[+T, +N](in: List[N], out: N, expr: T)            extends ParseElem[T, N]
  final case class MultiStart[+T, +N](in: List[N], m: N, expr: T, ar: Int) extends ParseElem[T, N]
  final case class MultiAdd[+N](m: N, out: N, idx: Int)                    extends ParseElem[Nothing, N]
  final case class Result[+N](res: List[N])                                extends ParseElem[Nothing, N]
  case object Split                                                        extends ParseElem[Nothing, Nothing]
  final case class Other[+T](t: T)                                         extends ParseElem[T, Nothing]
}

object parse {
  import ParseElem._
  type AssocL[T, N] = Assoc[T, List[N], List[N]]
  type Body[T, N]   = List[List[AssocL[T, N]]]
  type Parsed[T, N] = AssocL[Body[T, N], N]
  type Result[X, A] = Either[(Option[X], String), A]

  def collectBody[T, N, X](xs: List[X], in: List[N], parse: X => ParseElem[T, N]): Result[X, Parsed[T, N]] =
    xs.reverse match {
      case Nil => Left((None, "empty body"))
      case last :: restRev =>
        parse(last) match {
          case Result(out) =>
            restRev.reverse
              .foldLeftM[Result[X, ?], (Collect[T, N, N, N], Body[T, N])]((Collect(), Nil)) {
                case ((coll, acc), x) =>
                  parse(x) match {
                    case Single(in1, out, expr)      => (coll.addSingle(expr, in1, out), acc).asRight
                    case MultiStart(in, m, expr, ar) => (coll.startMultiIn(expr, in, m, ar), acc).asRight
                    case MultiAdd(m, out, idx)       => (coll.addMultiOut(m, idx, out), acc).asRight
                    case Split                       => (Collect[T, N, N, N](), coll.assocs :: acc).asRight
                    case Result(_)                   => (Some(x), "too early result").asLeft
                    case Other(_)                    => (Some(x), "unknown expression").asLeft
                  }
              }
              .map {
                case (coll, acc) => Assoc((if (coll.isEmpty) coll.assocs :: acc else acc).reverse, in = in, out = out)
              }

          case _ => (Some(last), "final statement should be result").asLeft
        }
    }
}
