package volga

import cats.Functor
import cats.data.StateT
import cats.instances.either._
import cats.instances.list._
import cats.instances.set._
import cats.kernel.Monoid
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.monoid._
import cats.syntax.option._
import cats.syntax.traverse._
import monocle.PLens
import monocle.function.all._
import monocle.macros.{Lenses, PLenses}
import monocle.syntax.apply._

import scala.annotation.tailrec

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
    singles.map {
      _.fold(m => multis(m).modOut(_.toList.flatten), identity)
    }.reverse

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
                                      parse: (X, Boolean) => (M, ParseElem[T, N])): Result[X, (M, Parsed[T, N])] = {
    def res(mi: M, out: List[N], restRev: List[X], last: Option[(T, X, List[N], N)]): Result[X, (M, Parsed[T, N])] = {
      val start = (Collect(): Collect[(T, X), N, N, N], Nil: Body[(T, X), N], mi)

      restRev.reverse
        .foldLeftM(start) {
          case ((coll, acc, ms), x) =>
            val (me, res) = parse(x, false)
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
          case (coll, acc, m) =>
            val coll1 = last.fold(coll) { case (t, x, ins, out) => coll.addSingle(t -> x, ins, out) }
            splitUsage(prependUnless(coll1.assocs, acc, coll1.isEmpty).reverse, in).map { bodyX =>
              val body = Functor[List].compose[List].map(bodyX)(Assoc.app1.modify(_._1))
              (m, Assoc(body, in = in, out = out))
            }
        }
    }

    xs.reverse match {
      case Nil => Left((None, "empty body"))
      case last :: restRev =>
        parse(last, true) match {
          case (mi, Result(out))                    => res(mi, out, restRev, None)
          case (mi, MultiStart(lastIn, m, expr, 0)) => res(mi, List(m), restRev, (expr, last, lastIn, m).some)

          case _ => (last.some, "final statement should be result expr or call").asLeft
        }
    }
  }

  def addLaterUse[T, N](p: Parsed[T, N]): Parsed[Option[T], N] =
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

  def preventReuse[T, N](p: Parsed[T, N]): (List[(T, N)], List[(Option[T], N)]) = {
    val (s, u, re) = p.app.flatten
      .foldLeft((p.in.map(_ -> none[T]).toMap, Set[N](), List[(T, N)]())) {
        case ((seen, used, acc), Assoc(app, i, o)) =>
          (
            seen ++ o.tupleRight(Some(app)),
            used ++ i,
            i.filter(used).tupleLeft(app) ::: acc
          )
      }
    (re, (s -- u).toList.map(_.swap))

  }

  //    p.modApp(
  //      _.foldRight((p.out.toSet, List[Block[Option[T], N]]())) {
  //        case (block, (need, acc)) =>
  //          val provides    = (block: List[AssocL[T, N]]).foldMap(_.out.toSet)
  //          val pass        = need -- provides
  //          val mappedBlock = block.map(_.modApp(_.some))
  //          val next        = (block: List[AssocL[T, N]]).foldMap(_.in.toSet) ++ pass
  //          (next, prependUnless(Assoc(none[T], pass.toList, pass.toList), mappedBlock, pass.isEmpty) :: acc)
  //      }._2
  //    )

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
