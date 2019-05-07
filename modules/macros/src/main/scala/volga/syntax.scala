package volga

import cats.Functor
import cats.data.StateT
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.traverse._
import monocle.PLens
import monocle.function.all._
import monocle.macros.{Lenses, PLenses}
import monocle.syntax.apply._

import scala.annotation.tailrec
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
            val lm                   = ls.toList.map(matchElem)
            val xns                  = xs.collect { case ValDef(_, name, _, _) => name }.toList
            val ps                   = parse.collectBody(ls.toList, xns, matchElem)
            val Assoc(body, in, out) = ps.fold({ case (to, x) => c.abort(to.fold(c.enclosingPosition)(_.pos), x) }, identity)
            val bodspl               = body.map(_.map { case Assoc(b, ins, outs) => s"$outs <- $b -< $ins" }).intercalate(List("<<SPLIT>>"))
            in :: out :: bodspl
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
    def apply[X1, VB](v: V[X1])(implicit vb: Vars[B, VB], ev: X1 <:< A): VB                                          = vb.va
    def apply[X1, X2, VB](v1: V[X1], v2: V[X2])(implicit vb: Vars[B, VB], ev: (X1, X2) <:< A): VB                    = vb.va
    def apply[X1, X2, X3, VB](v1: V[X1], v2: V[X2], v3: V[X3])(implicit vb: Vars[B, VB], ev: (X1, X2, X3) <:< A): VB = vb.va
    def apply[X1, X2, X3, X4, VB](v1: V[X1], v2: V[X2], v3: V[X3], v4: V[X4])(implicit vb: Vars[B, VB],
                                                                              ev: (X1, X2, X3, X4) <:< A): VB = vb.va

    def app[X1, VB](v: V[X1])(implicit vb: Vars[B, VB], ev: X1 <:< A): VB                                          = vb.va
    def app[X1, X2, VB](v1: V[X1], v2: V[X2])(implicit vb: Vars[B, VB], ev: (X1, X2) <:< A): VB                    = vb.va
    def app[X1, X2, X3, VB](v1: V[X1], v2: V[X2], v3: V[X3])(implicit vb: Vars[B, VB], ev: (X1, X2, X3) <:< A): VB = vb.va
    def app[X1, X2, X3, X4, VB](v1: V[X1], v2: V[X2], v3: V[X3], v4: V[X4])(implicit vb: Vars[B, VB],
                                                                            ev: (X1, X2, X3, X4) <:< A): VB = vb.va
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

@PLenses
final case class Assoc[A, I, O](app: A, in: I, out: O) {
  def modOut[O1](f: O => O1) = copy(out = f(out))
}
object Assoc {
  def out1[A, I, O, O1]: PLens[Assoc[A, I, O], Assoc[A, I, O1], O, O1] = out
  def app1[A, I, O, A1]: PLens[Assoc[A, I, O], Assoc[A1, I, O], A, A1] = app
}

@Lenses
final case class Collect[A, M, I, O](singles: List[Either[M, Assoc[A, List[I], List[O]]]] = Nil,
                                     multis: Map[M, Assoc[A, List[I], Vector[Option[O]]]] = Map.empty[M, Nothing]) {
  private def add(s: Either[M, Assoc[A, List[I], List[O]]]) =
    this &|-> Collect.singles modify (s :: _)

  def addSingle(app: A, ins: List[I], out: O) =
    add(Right(Assoc(app, ins, List(out))))
  def startMultiIn(app: A, ins: List[I], m: M, arity: Int) =
    add(Left(m)) &|-> Collect.multis ^|-> at(m) set Some(Assoc(app, ins, Vector.fill[Option[O]](arity)(None)))
  def addMultiOut(m: M, idx: Int, out: O) =
    this &|-> Collect.multis ^|-? index(m) ^|-> Assoc.out1 ^|-? index(idx) set Some(out)

  def assocs: List[Assoc[A, List[I], List[O]]] =
    singles.map { _.fold(m => multis(m).modOut(_.toList.flatten), identity) }.reverse

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
  type Block[T, N]  = List[AssocL[T, N]]
  type Body[T, N]   = List[Block[T, N]]
  type Parsed[T, N] = AssocL[Body[T, N], N]
  type Result[X, A] = Either[(Option[X], String), A]

  private def prependUnless[A](xs: A, xss: List[A], p: Boolean): List[A] = if (p) xss else xs :: xss

  def splitUsage[T, N, X](body: Body[(T, X), N], in: List[N]): Result[X, Body[(T, X), N]] = {
    @tailrec def walk(known: Set[N])(items: Block[(T, X), N],
                                     locally: Set[N] = known,
                                     cur: Block[(T, X), N] = List(),
                                     acc: Body[(T, X), N] = List()): Result[X, (Set[N], Body[(T, X), N])] =
      items match {
        case Nil => (locally, prependUnless(cur.reverse, acc, cur.isEmpty).reverse).asRight
        case (a @ Assoc((t, x), in1, out)) :: rest =>
          if (in1.forall(known)) walk(known)(rest, locally ++ out, a :: cur, acc)
          else if (in1.forall(locally)) walk(locally)(rest, locally ++ out, List(a), cur.reverse :: acc)
          else (Some(x), s"unknown input: ${in1.filterNot(locally).mkString(", ")} $known $locally").asLeft
      }

    (body: List[List[AssocL[(T, X), N]]]).flatTraverse(assocs => StateT(walk(_: Set[N])(assocs))).runA(in.toSet)
  }

  def collectBody[T, N, X](xs: List[X], in: List[N], parse: X => ParseElem[T, N]): Result[X, Parsed[T, N]] =
    xs.reverse match {
      case Nil => Left((None, "empty body"))
      case last :: restRev =>
        parse(last) match {
          case Result(out) =>
            restRev.reverse
              .foldLeftM[Result[X, ?], (Collect[(T, X), N, N, N], Body[(T, X), N])]((Collect(), Nil)) {
                case ((coll, acc), x) =>
                  parse(x) match {
                    case Single(in1, out1, expr)      => (coll.addSingle(expr -> x, in1, out1), acc).asRight
                    case MultiStart(in1, m, expr, ar) => (coll.startMultiIn(expr -> x, in1, m, ar), acc).asRight
                    case MultiAdd(m, out1, idx)       => (coll.addMultiOut(m, idx, out1), acc).asRight
                    case Split                        => (Collect[(T, X), N, N, N](), coll.assocs :: acc).asRight
                    case Result(_)                    => (Some(x), "too early result").asLeft
                    case Other(_)                     => (Some(x), "unknown expression").asLeft
                  }
              }
              .flatMap { case (coll, acc) => splitUsage(prependUnless(coll.assocs, acc, coll.isEmpty).reverse, in) }
              .map { bodyX =>
                val body = Functor[List].compose[List].map(bodyX)(Assoc.app1.modify(_._1))
                Assoc(body, in = in, out = out)
              }

          case _ => (Some(last), "final statement should be result").asLeft
        }
    }
}
