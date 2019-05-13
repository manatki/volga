package volga.solve
import cats.syntax.foldable._
import cats.syntax.apply._
import cats.syntax.option._
import cats.syntax.either._
import cats.instances.vector._
import cats.instances.either._
import volga.solve.Bin._
import volga.solve.BinOp._

import scala.annotation.tailrec

sealed trait Bin[+A] {
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
}

object Bin {
  final case class Branch[+A](l: Bin[A], r: Bin[A]) extends Bin[A] {
    def apply(side: Side) = side match {
      case L => l
      case R => r
    }
  }
  final case class Leaf[+A](a: A) extends Bin[A]

}

sealed trait BinOp {
  def exchange(other: BinOp): BinOp.Exchange

  def invert: BinOp = this match {
    case RotateL     => RotateR
    case RotateR     => RotateL
    case Swap        => Swap
    case Split(l, r) => Split(l.invertAll, r.invertAll)
  }
}

object BinOp {
  implicit class BinOpOps(private val binops: Vector[BinOp]) extends AnyVal {
    def invertAll: Vector[BinOp]                           = binops.reverseMap(_.invert)
    def merge(next: TraversableOnce[BinOp]): Vector[BinOp] = next.foldLeft(binops)(_ mergeOne _)
    def mergeOne(op: BinOp): Vector[BinOp]                 = mergeOneLoop(op, Vector())

    @tailrec private[BinOpOps] def mergeOneLoop(op: BinOp, rest: Vector[BinOp]): Vector[BinOp] = binops match {
      case Vector() => op +: rest
      case init :+ last =>
        val Exchange(cont, res) = last.exchange(op)
        res match {
          case Vector()             => binops ++ res ++ rest
          case head +: tail if cont => init.mergeOneLoop(head, tail ++ rest)
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
    def exchange(other: BinOp): Exchange = other match {
      case RotateL | RotateR => Stop(this, other)
      case Swap              => Continue(Split(right, left), Swap)
      case Split(lo, ro)     => Continue(Split(left ++ lo, right ++ ro))
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
}

final case class BinZipper[A](
    history: Vector[BinOp],
    parent: Option[(Side, BinZipper[A])],
    bin: Bin[A]
) {
  def goUp = parent.collect {
    case (L, BinZipper(ph, pp, Branch(_, r))) =>
      BinZipper(ph.mergeOne(Split(history, Vector())), pp, Branch(bin, r))
    case (R, BinZipper(ph, pp, Branch(l, _))) =>
      BinZipper(ph.mergeOne(Split(Vector(), history)), pp, Branch(l, bin))
  }

  def go(side: Side) = Some(bin) collect {
    case b @ Branch(_, _) => BinZipper(Vector(), (side, this).some, b(side))
  }
  def goLeft  = go(L)
  def goRight = go(R)

  def rotateL = Some(bin) collect {
    case Branch(l, Branch(rl, rr)) => BinZipper(history :+ RotateL, parent, Branch(Branch(l, rl), rr))
  }

  def rotateR = Some(bin) collect {
    case Branch(Branch(ll, lr), r) => BinZipper(history :+ RotateR, parent, Branch(ll, Branch(lr, r)))
  }

  def swap = Some(bin) collect {
    case Branch(l, r) => BinZipper(history :+ Swap, parent, Branch(r, l))
  }
}
