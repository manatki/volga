package volga

import cats.Functor
import cats.arrow.Arrow
import cats.data.StateT
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.instances.set._
import cats.instances.map._
import cats.kernel.Monoid
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.traverse._
import monocle.PLens
import monocle.function.all._
import monocle.macros.{Lenses, PLenses}
import monocle.syntax.apply._
import cats.syntax.option._
import cats.syntax.monoid._
import cats.syntax.functor._

import scala.annotation.tailrec
import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

class syntaxMacro(val c: blackbox.Context) {
  import c.universe._
  val syntSym = reify(syntax).tree.symbol
  object Syntax {
    def unapply(tree: Tree): Boolean = tree.symbol == syntSym
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

  def sarr[P: WeakTypeTag](body: c.Tree)(vb: Tree): c.Tree = arr[P](body)

  private def constructArrConnect(conn: parse.Connect[TermName]): Tree = {
    val (ins, outs) = conn
    val inpat       = ins.map(names => pq"(..$names)").reduce((a, b) => pq"($a, $b)")
    val outres      = outs.map(names => q"(..$names)").reduce((a, b) => q"($a, $b)")
    val param       = c.freshName[TermName]("input")
    val parin       = q"val $param = $EmptyTree"
    q"$syntSym.liftf ($parin => $param match { case $inpat => $outres })"
  }

  def getOrPass(types: Map[TermName, Type], P: Type)(a: Assoc[Option[Tree], List[TermName], List[TermName]]) =
    a.app.getOrElse(q"$syntSym.ident[$P, (..${a.in.map(types)})]")

  def arr[P: WeakTypeTag](body: c.Tree): c.Tree = {
    val P = weakTypeOf[P].typeConstructor
    val elems: List[Any] = body match {
      case q"(..$xs) => $b" =>
        b match {
          case q"{..$ls}" =>
            val lm                   = ls.toList.map(matchElem)
            val xns                  = xs.collect { case ValDef(_, name, _, _) => name }.toList
            val ps                   = parse.collectBody(ls.toList, xns, matchElem)
            val (types, parsed)      = ps.fold({ case (to, x) => c.abort(to.fold(c.enclosingPosition)(_.pos), x) }, identity)
            val withReuse            = parse.addReuse(parsed)
            val connects             = parse.inOuts(withReuse).map(constructArrConnect)
            val typeMap              = types.toMap
            val flow                 = withReuse.app.map(_.map(getOrPass(typeMap, P)).reduce((a, b) => q"($a.split($b))"))
            val res                  = parse.alternate(connects, flow).reduce((x, y) => q"($x.andThen($y))")
            val Assoc(body, in, out) = withReuse

            val bodspl = body
              .map(_.map { case Assoc(b, ins, outs) => s"$outs <- ${b.getOrElse("<<REUSE>>")} -< $ins" })
              .intercalate(List("<<SPLIT>>"))
            types :: res :: in :: out :: bodspl
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

  def matchElem[A](t: Tree): (List[(TermName, Type)], ParseElem[Tree, TermName]) = t match {
    case ValDef(_, name, q"${t: Type}", ArrSyn(smth, args)) => (List(name -> t), ParseElem.Single(args, name, smth))
    case ValDef(_, name, q"${t: Type}", VarElem(v, i))      => (List(name -> t), ParseElem.MultiAdd(v, name, i - 1))
    case ValDef(mods, name, t, Match(ArrSyn(smth, args), _)) if mods.hasFlag(Flag.SYNTHETIC) =>
      val arity = t.tpe match {
        case TypeRef(_, _, xs) => xs.length
        case _                 => 0
      }
      (List(), ParseElem.MultiStart(args, name, smth, arity))
    case q"${Split()}"          => (List(), ParseElem.Split)
    case q"(..${Names(names)})" => (List(), ParseElem.Result(names))
    case l                      => (List(), ParseElem.Other(l))
  }
}

object syntax {
  class V[X]

  private object varro extends V[Any]
  private def varr[X]: V[X] = varro.asInstanceOf[V[X]]

  object ----

  def arr[P[_, _]] = new MkArr[P]

  def ident[P[_, _], A](implicit arr: Arrow[P]): P[A, A]               = arr.id
  def liftf[P[_, _], A, B](f: A => B)(implicit arr: Arrow[P]): P[A, B] = arr.lift(f)

  class MkArr[P[_, _]] {
    def apply[VB, B](body: () => VB)(implicit vb: Vars[B, VB]): P[Unit, B] = macro syntaxMacro.sarr[P[Unit, Unit]]

    def apply[A, VB, B](body: V[A] => VB)(implicit vb: Vars[B, VB]): P[A, B] = macro syntaxMacro.sarr[P[Unit, Unit]]

    def apply[A1, A2, VB, B](body: (V[A1], V[A2]) => VB)(implicit vb: Vars[B, VB]): P[(A1, A2), B] =
      macro syntaxMacro.sarr[P[Unit, Unit]]

    def apply[A1, A2, A3, VB, B](body: (V[A1], V[A2], V[A3]) => VB)(implicit vb: Vars[B, VB]): P[(A1, A2, A3), B] =
      macro syntaxMacro.sarr[P[Unit, Unit]]

    def apply[A1, A2, A3, A4, VB, B](body: (V[A1], V[A2], V[A3], V[A4]) => VB)(implicit vb: Vars[B, VB]): P[(A1, A2, A3), B] =
      macro syntaxMacro.sarr[P[Unit, Unit]]
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
  def modApp[A1](f: A => A1) = copy(app = f(app))
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
  type Ports[N]     = List[List[N]]
  type Connect[N]   = (Ports[N], Ports[N])

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

  def collectBody[T, N, X, M: Monoid](xs: List[X],
                                      in: List[N],
                                      parse: X => (M, ParseElem[T, N])): Result[X, (M, Parsed[T, N])] =
    xs.reverse match {
      case Nil => Left((None, "empty body"))
      case last :: restRev =>
        parse(last) match {
          case (mi, Result(out)) =>
            restRev.reverse
              .foldLeftM[Result[X, ?], (Collect[(T, X), N, N, N], Body[(T, X), N], M)]((Collect(), Nil, mi)) {
                case ((coll, acc, ms), x) =>
                  val (me, res) = parse(x)
                  val mn        = ms |+| me
                  res match {
                    case Single(in1, out1, expr)      => (coll.addSingle(expr -> x, in1, out1), acc, mn).asRight
                    case MultiStart(in1, m, expr, ar) => (coll.startMultiIn(expr -> x, in1, m, ar), acc, mn).asRight
                    case MultiAdd(m, out1, idx)       => (coll.addMultiOut(m, idx, out1), acc, mn).asRight
                    case Split                        => (Collect[(T, X), N, N, N](), coll.assocs :: acc, mn).asRight
                    case Result(_)                    => (x.some, "too early result").asLeft
                    case Other(_)                     => (x.some, "unknown expression").asLeft
                  }
              }
              .flatMap {
                case (coll, acc, m) => splitUsage(prependUnless(coll.assocs, acc, coll.isEmpty).reverse, in).tupleRight(m)
              }
              .map {
                case (bodyX, m) =>
                  val body = Functor[List].compose[List].map(bodyX)(Assoc.app1.modify(_._1))
                  (m, Assoc(body, in = in, out = out))
              }

          case _ => (last.some, "final statement should be result").asLeft
        }
    }

  def addReuse[T, N](p: Parsed[T, N]): Parsed[Option[T], N] =
    p.modApp(
      _.foldRight((p.out.toSet, List[Block[Option[T], N]]())) {
        case (block, (need, acc)) =>
          val provides    = (block: List[AssocL[T, N]]).foldMap(_.out.toSet)
          val pass        = need -- provides
          val mappedBlock = block.map(_.modApp(_.some))
          val next        = (block: List[AssocL[T, N]]).foldMap(_.in.toSet) ++ pass
          (next, prependUnless(Assoc(none[T], pass.toList, pass.toList), mappedBlock, pass.isEmpty) :: acc)
      }._2
    )

  def inOuts[T, N](p: Parsed[T, N]): List[Connect[N]] = {
    val ins  = List(p.in) :: p.app.map(_.map(_.out))
    val outs = p.app.map(_.map(_.in)) :+ List(p.out)
    ins.zip(outs)
  }

  def alternate[A](xs: List[A], ys: List[A]): List[A] = {
    def go(xs1: List[A], ys1: List[A], acc: List[A]): List[A] = xs1 match {
      case x :: rest => go(ys1, rest, x :: acc)
      case Nil       => acc reverse_::: ys1
    }
    go(xs, ys, Nil)
  }
}
