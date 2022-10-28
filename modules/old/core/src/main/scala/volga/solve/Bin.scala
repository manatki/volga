package volga.solve
import cats.instances.either._
import cats.instances.option._
import cats.instances.string._
import cats.instances.vector._
import cats.kernel.Semigroup
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.monad._
import PMagma.ops._
import cats.syntax.option._
import cats.syntax.show._
import cats.{Applicative, Eval, Functor, Monoid, Show, Traverse}
import volga.solve.Bin._
import volga.solve.BinHistory.{HChain, HConsume, HGrow, HRotate, HSplit, HSwap}
import volga.solve.BinOp._
import cats.syntax.applicative._

import scala.annotation.tailrec

sealed trait Bin[+A] {
  def elems: Vector[A] = this.toList.toVector

  def mod(binOp: BinOp): Either[Error, Bin[A]] = {
    def fail = Error(binOp, this).asLeft

    binOp match {
      case Grow(L) => Branch(Bud, this).asRight
      case Grow(R) => Branch(this, Bud).asRight
      case _ =>
        this match {
          case Leaf(_) | Bud => fail
          case Branch(l, r) =>
            binOp match {
              case Swap => Branch(r, l).asRight
              case Split(ls, rs) =>
                (l.modAll(ls).leftMap(L :: _), r.modAll(rs).leftMap(R :: _)).mapN(Branch(_, _))
              case Rotate(R) =>
                l match {
                  case Leaf(_) | Bud  => fail
                  case Branch(ll, lr) => Branch(ll, Branch(lr, r)).asRight
                }
              case Rotate(L) =>
                r match {
                  case Leaf(_) | Bud  => fail
                  case Branch(rl, rr) => Branch(Branch(l, rl), rr).asRight
                }
              case Consume(L) =>
                l match {
                  case Bud => r.asRight
                  case _   => fail
                }
              case Consume(R) =>
                r match {
                  case Bud => l.asRight
                  case _   => fail
                }
              case Grow(_) => fail
            }
        }
    }
  }

  def modAll(vs: Vector[BinOp]): Either[Error, Bin[A]] =
    vs.zipWithIndex
      .foldM(this) {
        case (b, (op, i)) => b.mod(op).leftMap(At(i) :: _)
      }

  def zipper = BinZipper(this)

  def adaptation[A1 >: A](bin: Bin[A1]): Either[String, Vector[BinOp]] =
    Permutations.buildPerm(elems, bin.elems).leftMap(_.mkString_(",")).flatMap { perm =>
      Permutations
        .swaps(perm)
        .foldM(zipper.normalize) {
          case (z, (i, j)) => z.swapElems(i, j).liftTo[Either[String, *]](s"Bad state while swapping $i <-> $j")
        }
        .map(_.history ++ bin.zipper.normalize.history.invertAll)
    }
}

object Bin {
  final case class Branch[+A](l: Bin[A], r: Bin[A]) extends Bin[A] {
    def apply(side: Side) = side match {
      case L => l
      case R => r
    }
  }
  final case class Leaf[+A](a: A) extends Bin[A]
  case object Bud                 extends Bin[Nothing]

  implicit def show[A: Show]: Show[Bin[A]] = {
    case Leaf(a)      => a.show
    case Branch(l, r) => show"($l, $r)"
    case Bud          => "*"
  }

  implicit val traverse: Traverse[Bin] = new Traverse[Bin] {
    def traverse[G[_]: Applicative, A, B](fa: Bin[A])(f: A => G[B]): G[Bin[B]] =
      fa match {
        case Leaf(a)      => f(a).map(Leaf(_))
        case Branch(l, r) => traverse(l)(f).map2(traverse(r)(f))(Branch(_, _))
        case Bud          => (Bud: Bin[B]).pure[G]
      }
    def foldLeft[A, B](fa: Bin[A], b: B)(f: (B, A) => B): B = {
      @tailrec def go(b1: B, bin: Bin[A], stack: List[Bin[A]]): B =
        bin match {
          case Leaf(a) =>
            val b2 = f(b1, a)
            stack match {
              case head :: tail => go(b2, head, tail)
              case Nil          => b2
            }
          case Branch(l, r) => go(b1, l, r :: stack)
          case Bud =>
            stack match {
              case head :: tail => go(b, head, tail)
              case Nil          => b
            }
        }
      go(b, fa, Nil)
    }
    def foldRight[A, B](fa: Bin[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match {
        case Leaf(a)      => f(a, lb)
        case Branch(l, r) => foldRight(l, Eval.defer(foldRight(r, lb)(f)))(f)
        case Bud          => lb
      }
  }
}

sealed trait BinOp {
  def tryExchange: PartialFunction[BinOp, BinOp.Exchange]

  def exchange(other: BinOp): BinOp.Exchange =
    tryExchange.applyOrElse(other, (_: BinOp) => Stop(this, other))

  def invert: BinOp = this match {
    case Rotate(side)  => Rotate(side.other)
    case Swap          => Swap
    case Split(l, r)   => Split(l.invertAll, r.invertAll)
    case Grow(side)    => Consume(side)
    case Consume(side) => Grow(side)
  }

  def isEmpty = false
}

object BinOp {
  implicit class BinOpOps(private val binops: Vector[BinOp]) extends AnyVal {
    def invertAll: Vector[BinOp]                       = binops.reverseIterator.map(_.invert).toVector
    def mergeAll(next: Iterable[BinOp]): Vector[BinOp] = next.foldLeft(binops)(_ merge _)
    def merge(op: BinOp): Vector[BinOp] =
      if (op.isEmpty) binops else mergeOneLoop(op, Vector())
    def optimize: Vector[BinOp] = Vector() mergeAll binops
    def put(op: BinOp)          = if (op.isEmpty) binops else binops :+ op

    @tailrec private[BinOpOps] def mergeOneLoop(op: BinOp, rest: Vector[BinOp]): Vector[BinOp] = binops match {
      case Vector() => op +: rest
      case init :+ last =>
        val Exchange(cont, res) = last.exchange(op)
        res match {
          case head +: tail if cont => init.mergeOneLoop(head, tail ++ rest)
          case _                    => init ++ res ++ rest
        }
    }
  }

  final case class Consume(side: Side) extends BinOp {
    def tryExchange = { case Grow(`side`) => Stop() }
  }

  final case class Grow(side: Side) extends BinOp {
    def tryExchange = { case Consume(`side`) => Stop() }
  }

  final case class Rotate(side: Side) extends BinOp {
    def tryExchange = { case Rotate(s1) if s1 != side => Stop() }
  }

  final case object Swap extends BinOp {
    def tryExchange = { case Swap => Stop() }
  }

  final case class Split(left: Vector[BinOp], right: Vector[BinOp]) extends BinOp {
    override val isEmpty = left.forall(_.isEmpty) && right.forall(_.isEmpty)

    def tryExchange = {
      case Swap          => Continue(Split(right, left), Swap)
      case Split(lo, ro) => Continue(Split(left mergeAll lo, right mergeAll ro))
    }
  }

  final case class Exchange(continue: Boolean, res: Vector[BinOp])

  def Stop(binOp: BinOp*)     = Exchange(false, binOp.toVector)
  def Continue(binOp: BinOp*) = Exchange(true, binOp.toVector)

  sealed trait Index
  case class At(i: Int) extends Index

  sealed trait Side extends Index {
    def other: Side
  }
  case object L extends Side {
    def other = R
  }
  case object R extends Side {
    def other = L
  }

  final case class Error(op: BinOp, tree: Bin[Any], path: List[Index] = Nil) {
    def ::(i: Index): Error     = copy(path = i :: path)
    def ~(bin: Bin[Any]): Error = copy(tree = bin)
  }

  final implicit val showError: Show[Error] = err => s"error ${err.op} : ${err.path.mkString("-")}"
}

final case class BinZipper[+A](
    bin: Bin[A],
    history: Vector[BinOp] = Vector(),
    parent: Option[(Side, BinZipper[A])] = None
) {
  def walk[A1 >: A](fs: (BinZipper[A1] => Option[BinZipper[A1]])*): Option[BinZipper[A1]] =
    fs.foldLeft[Option[BinZipper[A1]]](this.some)((bz, f) => bz.flatMap(f))

  def goUp =
    parent.collect {
      case (L, BinZipper(Branch(_, r), ph, pp)) =>
        BinZipper(Branch(bin, r), ph put Split(history, Vector()), pp)
      case (R, BinZipper(Branch(l, _), ph, pp)) =>
        BinZipper(Branch(l, bin), ph put Split(Vector(), history), pp)
    }

  def go(side: Side) =
    bin.some.collect {
      case b @ Branch(_, _) => BinZipper(b(side), Vector(), (side, this).some)
    }

  def goLeft  = go(L)
  def goRight = go(R)

  def grow(side: Side) = {
    val bin1 = side match {
      case L => Branch(Bud, bin)
      case R => Branch(bin, Bud)
    }
    BinZipper(bin1, history put Grow(side), parent).some
  }

  def consume(side: Side) =
    bin.some.collect {
      case Branch(Bud, t) if side == L => BinZipper(t, history put Consume(side), parent)
      case Branch(t, Bud) if side == R => BinZipper(t, history put Consume(side), parent)
    }

  def rotate(side: Side) =
    bin.some.collect {
      case Branch(l, Branch(rl, rr)) if side == L =>
        BinZipper(Branch(Branch(l, rl), rr), history put Rotate(L), parent)
      case Branch(Branch(ll, lr), r) if side == R =>
        BinZipper(Branch(ll, Branch(lr, r)), history put Rotate(R), parent)
    }

  def swap =
    bin.some.collect {
      case Branch(l, r) => BinZipper(Branch(r, l), history put Swap, parent)
    }

  def modify[A1 >: A](f: A => A1) =
    bin.some.collect {
      case Leaf(a) => BinZipper(Leaf(f(a)), history, parent)
    }

  def set[A1 >: A](a: A1) = modify(_ => a)

  def attempt[A1 >: A](f: Option[BinZipper[A1]]): BinZipper[A1] = f.getOrElse(this)

  def clean: BinZipper[A] =
    attempt(walk(_.goLeft, _.clean.some, _.goUp, _.goRight, _.clean.some, _.goUp))
      .attempt(consume(L))
      .attempt(consume(R))

  def linearize: BinZipper[A] =
    rotate(R) match {
      case Some(t) => t.linearize
      case None =>
        goRight match {
          case Some(b) => b.linearize
          case None    => this
        }
    }

  def normalize: BinZipper[A] = clean.linearize.top

  def top: BinZipper[A] = goUp match {
    case Some(p) => p.top
    case None    => this
  }

  def tree: Bin[A] = top.bin

  def appMany(ops: Vector[BinOp]): Option[BinZipper[A]] = ops.foldM(this)(_ app _)

  def app(op: BinOp): Option[BinZipper[A]] = op match {
    case Rotate(side)  => rotate(side)
    case Swap          => swap
    case Split(ls, rs) => walk(_.goLeft, _.appMany(ls), _.goUp, _.goRight, _.appMany(rs), _.goUp)
    case Consume(side) => consume(side)
    case Grow(side)    => grow(side)
  }

  def repeat[A1 >: A](count: Int)(act: BinZipper[A1] => Option[BinZipper[A1]]): Option[BinZipper[A1]] =
    (this: BinZipper[A1], count).iterateWhileM { case (z, c) => act(z).tupleRight(c - 1) }(_._2 > 0).map(_._1)

  def swapNext = rotate(L) match {
    case Some(z) => z.walk(_.goLeft, _.swap, _.goUp, _.rotate(R))
    case None    => swap
  }

  def swapElems(i: Int, j: Int) =
    if (i >= j) this.some
    else
      walk(
        _.repeat(i)(_.goRight),
        _.repeat(j - i - 1)(_.walk(_.swapNext, _.goRight)),
        _.swapNext,
        _.repeat(j - i - 1)(_.walk(_.goUp, _.swapNext)),
        _.repeat(i)(_.goUp)
      )
}

sealed trait BinHistory[+A]

object BinHistory {
  final case class HChain[+A](start: A, end: A, history: Vector[BinHistory[A]]) {
    def map[B](f: A => B): HChain[B] = HChain(f(start), f(end), history.map(_.map(f)))
  }
  final case class HRotate[+A](side: Side, l: A, m: A, r: A) extends BinHistory[A]
  final case class HSwap[+A](l: A, r: A)                     extends BinHistory[A]
  final case class HSplit[+A](
      left: HChain[A],
      right: HChain[A],
  ) extends BinHistory[A]
  final case class HConsume[+A](side: Side, v: A) extends BinHistory[A]
  final case class HGrow[+A](side: Side, v: A)    extends BinHistory[A]

  implicit val functor: Functor[BinHistory] = new Functor[BinHistory] {
    def map[A, B](fa: BinHistory[A])(f: A => B): BinHistory[B] = fa match {
      case HRotate(side, l, m, r) => HRotate(side, f(l), f(m), f(r))
      case HSwap(l, r)            => HSwap(f(l), f(r))
      case HSplit(ls, rs)         => HSplit(ls.map(f), rs.map(f))
      case HConsume(side, v)      => HConsume(side, f(v))
      case HGrow(side, v)         => HGrow(side, f(v))
    }
  }
}

sealed abstract class BinRes[+A] {
  def value: A

  def ifBranch[A1 >: A](op: BinOp)(f: (BinRes[A], BinRes[A]) => BinRes[A1]): BinRes[A1]

  def history: BinRes.History[A]

  def clean: BinRes[A]

  def withHistory[A1 >: A](prev: BinRes[A1], op: BinHistory[A1]): BinRes[A1]

  def success: Boolean
}

object BinRes {
  type History[+A] = Vector[BinHistory[A]]

  def apply[A: PMagma](bin: Bin[A]): BinRes[A] = bin match {
    case Leaf(a)      => LeafRes(a)
    case Branch(l, r) => BinRes(l) branch BinRes(r)
    case Bud          => LeafRes(PMagma[A].empty)
  }

  implicit class Binaops[A: PMagma](val b: BinRes[A]) {
    def branch(c: BinRes[A]): BinRes[A] = c match {
      case FailRes(v, h, ls) => FailRes(b.value ## c.value, h, ls)
      case _                 => BranchRes(b, c, b.value ## c.value, Vector())
    }

    def modAll(ops: Vector[BinOp]): BinRes[A] = ops.foldLeft(b)(_ mod _)

    def mod(op: BinOp): BinRes[A] =
      op match {
        case Grow(L) => LeafRes(PMagma[A].empty) branch b withHistory (b, HGrow(L, b.value))
        case Grow(R) => b branch LeafRes(PMagma[A].empty) withHistory (b, HGrow(R, b.value))
        case _ =>
          b.ifBranch(op)(
            (l, r) =>
              op match {
                case Swap => (r branch l) withHistory (b, HSwap(l.value, r.value))
                case Rotate(R) =>
                  l.ifBranch(op)(
                    (ll, lr) => ll.branch(lr.branch(r)) withHistory (b, HRotate(R, ll.value, lr.value, r.value))
                  )
                case Rotate(L) =>
                  r.ifBranch(op)(
                    (rl, rr) => l.branch(rl).branch(rr) withHistory (b, HRotate(L, l.value, rl.value, rr.value))
                  )
                case Split(ls, rs) =>
                  val lt = l.clean.modAll(ls)
                  val rt = r.clean.modAll(rs)
                  lt.clean branch rt.clean withHistory (b,
                  HSplit(
                    HChain(l.value, lt.value, lt.history),
                    HChain(r.value, rt.value, rt.history),
                  ))
                case Consume(L) => r.withHistory(b, HConsume(L, r.value))
                case Consume(R) => l.withHistory(b, HConsume(R, r.value))
                case Grow(_)    => b
              }
          )
      }
  }

  final case class LeafRes[+A](value: A) extends BinRes[A] {
    def ifBranch[A1 >: A](op: BinOp)(f: (BinRes[A], BinRes[A]) => BinRes[A1]): BinRes[A1] = FailRes(value, history, op)
    def history: History[A]                                                               = Vector()
    def clean: BinRes[A]                                                                  = this
    def withHistory[A1 >: A](prev: BinRes[A1], op: BinHistory[A1]): BinRes[A1]            = this
    def success                                                                           = true
  }
  final case class BranchRes[+A](l: BinRes[A], r: BinRes[A], value: A, history: History[A]) extends BinRes[A] {
    def ifBranch[A1 >: A](op: BinOp)(f: (BinRes[A], BinRes[A]) => BinRes[A1]): BinRes[A1] = f(l, r)
    def clean: BinRes[A]                                                                  = copy(history = Vector())
    def withHistory[A1 >: A](prev: BinRes[A1], op: BinHistory[A1]): BinRes[A1]            = copy(history = prev.history :+ op)
    def success                                                                           = true
  }
  final case class FailRes[+A](value: A, history: History[A], last: BinOp) extends BinRes[A] {
    def ifBranch[A1 >: A](op: BinOp)(f: (BinRes[A], BinRes[A]) => BinRes[A1]): BinRes[A1] = this
    def clean: BinRes[A]                                                                  = this
    def withHistory[A1 >: A](prev: BinRes[A1], op: BinHistory[A1]): BinRes[A1]            = this
    def success                                                                           = false
  }
}
