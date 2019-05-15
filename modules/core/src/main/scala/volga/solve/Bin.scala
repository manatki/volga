package volga.solve
import cats.data.NonEmptyList
import cats.{Applicative, Eval, Show, Traverse}
import cats.instances.either._
import cats.instances.option._
import cats.instances.vector._
import cats.instances.string._
import cats.syntax.functor._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.option._
import cats.syntax.show._
import cats.syntax.monad._
import volga.solve.Bin._
import volga.solve.BinOp._

import scala.annotation.tailrec

sealed trait Bin[+A] {
  def elems: Vector[A] = this.toList.toVector

  def mod(binOp: BinOp): Either[Error, Bin[A]] = {
    def fail = Error(binOp, this).asLeft

    this match {
      case Leaf(_) => fail
      case Branch(l, r) =>
        binOp match {
          case Swap => Branch(r, l).asRight
          case Split(ls, rs) =>
            (l.modAll(ls).leftMap(L :: _), r.modAll(rs).leftMap(R :: _)).mapN(Branch(_, _))
          case RotateR =>
            l match {
              case Leaf(_)        => fail
              case Branch(ll, lr) => Branch(ll, Branch(lr, r)).asRight
            }
          case RotateL =>
            r match {
              case Leaf(_)        => fail
              case Branch(rl, rr) => Branch(Branch(l, rl), rr).asRight
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
        .foldM(zipper.linearize.top) {
          case (z, (i, j)) => z.swapElems(i, j).liftTo[Either[String, ?]](s"Bad state while swapping $i <-> $j")
        }
        .map(_.history ++ bin.zipper.linearize.top.history.invertAll)
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

  implicit def show[A: Show]: Show[Bin[A]] = {
    case Leaf(a)      => a.show
    case Branch(l, r) => show"($l, $r)"
  }

  implicit val traverse: Traverse[Bin] = new Traverse[Bin] {
    def traverse[G[_]: Applicative, A, B](fa: Bin[A])(f: A => G[B]): G[Bin[B]] =
      fa match {
        case Leaf(a)      => f(a).map(Leaf(_))
        case Branch(l, r) => traverse(l)(f).map2(traverse(r)(f))(Branch(_, _))
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
        }
      go(b, fa, Nil)
    }
    def foldRight[A, B](fa: Bin[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match {
        case Leaf(a)      => f(a, lb)
        case Branch(l, r) => foldRight(l, Eval.defer(foldRight(r, lb)(f)))(f)
      }
  }

}

sealed trait BinOp {
  def exchange(other: BinOp): BinOp.Exchange

  def invert: BinOp = this match {
    case RotateL     => RotateR
    case RotateR     => RotateL
    case Swap        => Swap
    case Split(l, r) => Split(l.invertAll, r.invertAll)
  }

  def isEmpty = false
}

object BinOp {
  implicit class BinOpOps(private val binops: Vector[BinOp]) extends AnyVal {
    def invertAll: Vector[BinOp]                              = binops.reverseMap(_.invert)
    def mergeAll(next: TraversableOnce[BinOp]): Vector[BinOp] = next.foldLeft(binops)(_ merge _)
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

  final case object RotateL extends BinOp {
    def exchange(other: BinOp): Exchange = other match {
      case RotateR                      => Stop()
      case RotateL | Split(_, _) | Swap => Stop(this, other)
    }
  }
  final case object RotateR extends BinOp {
    def exchange(other: BinOp): Exchange = other match {
      case RotateL                      => Stop()
      case RotateR | Split(_, _) | Swap => Stop(this, other)
    }
  }
  final case object Swap extends BinOp {
    def exchange(other: BinOp): Exchange = other match {
      case Swap                            => Stop()
      case RotateR | RotateL | Split(_, _) => Stop(other, this)
    }
  }

  final case class Split(left: Vector[BinOp], right: Vector[BinOp]) extends BinOp {
    override val isEmpty = left.forall(_.isEmpty) && right.forall(_.isEmpty)

    def exchange(other: BinOp): Exchange = other match {
      case RotateL | RotateR => Stop(this, other)
      case Swap              => Continue(Split(right, left), Swap)
      case Split(lo, ro)     => Continue(Split(left mergeAll lo, right mergeAll ro))
    }
  }

  final case class Exchange(continue: Boolean, res: Vector[BinOp])

  def Stop(binOp: BinOp*)     = Exchange(false, binOp.toVector)
  def Continue(binOp: BinOp*) = Exchange(true, binOp.toVector)

  sealed trait Index
  case class At(i: Int) extends Index

  sealed trait Side extends Index
  case object L     extends Side
  case object R     extends Side

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

  def rotateL =
    bin.some.collect {
      case Branch(l, Branch(rl, rr)) => BinZipper(Branch(Branch(l, rl), rr), history put RotateL, parent)
    }

  def rotateR =
    bin.some.collect {
      case Branch(Branch(ll, lr), r) => BinZipper(Branch(ll, Branch(lr, r)), history put RotateR, parent)
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

  def linearize: BinZipper[A] =
    rotateR match {
      case Some(t) => t.linearize
      case None =>
        goRight match {
          case Some(b) => b.linearize
          case None    => this
        }
    }

  def top: BinZipper[A] = goUp match {
    case Some(p) => p.top
    case None    => this
  }

  def tree: Bin[A] = top.bin

  def appMany(ops: Vector[BinOp]): Option[BinZipper[A]] = ops.foldM(this)(_ app _)

  def app(op: BinOp): Option[BinZipper[A]] = op match {
    case RotateL       => rotateL
    case RotateR       => rotateR
    case Swap          => swap
    case Split(ls, rs) => walk(_.goLeft, _.appMany(ls), _.goUp, _.goRight, _.appMany(rs), _.goUp)
  }

  def repeat[A1 >: A](count: Int)(act: BinZipper[A1] => Option[BinZipper[A1]]): Option[BinZipper[A1]] =
    (this: BinZipper[A1], count).iterateWhileM { case (z, c) => act(z).tupleRight(c - 1) }(_._2 > 0).map(_._1)

  def swapNext = rotateL match {
    case Some(z) => z.walk(_.goLeft, _.swap, _.goUp, _.rotateR)
    case None    => swap
  }

  def swapElems(i: Int, j: Int) =
    if (i >= j) this.some
    else
      walk(_.repeat(i)(_.goRight),
           _.repeat(j - i - 1)(_.walk(_.swapNext, _.goRight)),
           _.swapNext,
           _.repeat(j - i - 1)(_.walk(_.goUp, _.swapNext)),
           _.repeat(i)(_.goUp))

}
