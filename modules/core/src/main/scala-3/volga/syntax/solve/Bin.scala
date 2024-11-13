package volga.syntax.solve

import volga.syntax.solve.Bin.*
import volga.syntax.solve.BinHistory.{HChain, HConsume, HGrow, HRotate, HSplit, HSwap}
import volga.syntax.solve.BinOp.*
import volga.syntax.solve.binop.*

import scala.annotation.tailrec
import scala.collection.Factory
import volga.util.collections.*
import volga.functors.Traverse
import volga.functors.Functor

enum Bin[+A]:
    case Branch(l: Bin[A], r: Bin[A])
    case Leaf(a: A)
    case Bud

    def elems: Vector[A] = this.to(Vector)

    def mod(binOp: BinOp): Either[Error, Bin[A]] =
        def fail = Left(Error(binOp, this))

        binOp match
            case Grow(L) => Right(Branch(Bud, this))
            case Grow(R) => Right(Branch(this, Bud))
            case _       =>
                this match
                    case Leaf(_) | Bud => fail
                    case Branch(l, r)  =>
                        binOp match
                            case Swap          => Right(Branch(r, l))
                            case Split(ls, rs) =>
                                for
                                    lb <- l.modAll(ls).left.map(L :: _)
                                    rb <- r.modAll(rs).left.map(R :: _)
                                yield Branch(lb, rb)
                            case Rotate(R)     =>
                                l match
                                    case Leaf(_) | Bud  => fail
                                    case Branch(ll, lr) => Right(Branch(ll, Branch(lr, r)))
                            case Rotate(L)     =>
                                r match
                                    case Leaf(_) | Bud  => fail
                                    case Branch(rl, rr) => Right(Branch(Branch(l, rl), rr))
                            case Consume(L)    =>
                                l match
                                    case Bud => Right(r)
                                    case _   => fail
                            case Consume(R)    =>
                                r match
                                    case Bud => Right(l)
                                    case _   => fail
                            case Grow(_)       => fail
        end match
    end mod

    def modAll(vs: Vector[BinOp]): Either[Error, Bin[A]] =
        vs.zipWithIndex.foldErr(this) { case (b, (op, i)) =>
            b.mod(op).left.map(At(i) :: _)
        }

    def zipper = BinZipper(this)

    def adaptation[A1](bin: Bin[A1]): Either[String, Vector[BinOp]] =
        for
            perm        <- Permutations.buildPerm(elems, bin.elems).left.map(_.mkString(","))
            swaps        = Permutations.swaps(perm)
            transformed <- swaps.foldErr(zipper.normalize) { case (z, (i, j)) =>
                               z.swapElems(i, j).toRight[String](s"Bad state while swapping $i <-> $j")
                           }
            full         = transformed.history ++ bin.zipper.normalize.history.invertAll
        yield full.optimize

    override def toString = this.match
        case Leaf(a)      => a.toString
        case Branch(l, r) => s"($l, $r)"
        case Bud          => "*"

    def to[C](factory: Factory[A, C]): C =
        val builder             = factory.newBuilder
        def go(b: Bin[A]): Unit =
            b match
                case Branch(l, r) =>
                    go(l)
                    go(r)
                case Leaf(a)      => builder += a
                case Bud          =>
        go(this)
        builder.result()
    end to
end Bin

object Bin:
    extension [A](b: Branch[A])
        def apply(s: Side) = s match
            case L => b.l
            case R => b.r

    def fromElements[A](xs: Iterable[A]): Bin[A] =
        if xs.isEmpty then Bud
        else xs.view.map(Leaf(_)).reduce(Branch(_, _))

    given Traverse[Bin] = Traverse.derived[Bin]

end Bin

package binop:
    sealed trait Index
    case class At(i: Int) extends Index

    sealed trait Side(val other: Side) extends Index
    case object L                      extends Side(other = R)
    case object R                      extends Side(other = L)

enum BinOp:
    case Consume(side: Side)
    case Grow(side: Side)
    case Rotate(side: Side)
    case Swap
    case Split(left: Vector[BinOp], right: Vector[BinOp])

    final def tryExchange: PartialFunction[BinOp, BinOp.Exchange] = this match
        case Consume(side)      => { case Grow(`side`) => Stop() }
        case Grow(side)         => { case Consume(`side`) => Stop() }
        case Rotate(side)       => { case Rotate(s1) if s1 != side => Stop() }
        case Swap               => { case Swap => Stop() }
        case Split(left, right) => {
            case Swap          => Continue(Split(right, left), Swap)
            case Split(lo, ro) => Continue(Split(left.mergeAll(lo), right.mergeAll(ro)))
        }

    def exchange(other: BinOp): BinOp.Exchange =
        tryExchange.applyOrElse(other, (_: BinOp) => Stop(this, other))

    def invert: BinOp = this match
        case Rotate(side)  => Rotate(side.other)
        case Swap          => Swap
        case Split(l, r)   => Split(l.invertAll, r.invertAll)
        case Grow(side)    => Consume(side)
        case Consume(side) => Grow(side)

    final def isEmpty: Boolean = this match
        case Split(left, right)                  => left.forall(_.isEmpty) && right.forall(_.isEmpty)
        case Swap | _: (Consume | Grow | Rotate) => false
end BinOp

object BinOp:
    import binop.*

    final case class Exchange(continue: Boolean, res: Vector[BinOp])
    extension (binops: Vector[BinOp])
        def invertAll: Vector[BinOp]                       = binops.reverseIterator.map(_.invert).toVector
        def mergeAll(next: Iterable[BinOp]): Vector[BinOp] = next.foldLeft(binops)(_.merge(_))
        def merge(op: BinOp): Vector[BinOp]                =
            if (op.isEmpty) binops else mergeOneLoop(op, Vector())
        def optimize: Vector[BinOp]                        = Vector().mergeAll(binops)
        def put(op: BinOp)                                 = if (op.isEmpty) binops else binops :+ op

        @tailrec private[BinOp] def mergeOneLoop(op: BinOp, rest: Vector[BinOp]): Vector[BinOp] = binops match
            case init :+ last =>
                val Exchange(cont, res) = last.exchange(op)
                res match
                    case head +: tail if cont => init.mergeOneLoop(head, tail ++ rest)
                    case _                    => init ++ res ++ rest
            case empty        => op +: rest
    end extension

    def Stop(binOp: BinOp*)     = Exchange(false, binOp.toVector)
    def Continue(binOp: BinOp*) = Exchange(true, binOp.toVector)

    final case class Error(op: BinOp, tree: Bin[Any], path: List[Index] = Nil) extends Throwable:
        def ::(i: Index): Error         = copy(path = i :: path)
        override def toString(): String = s"error $op : ${path.mkString("-")}"

end BinOp

final case class BinZipper[+A](
    bin: Bin[A],
    history: Vector[BinOp] = Vector(),
    parent: Option[(Side, BinZipper[A])] = None
):
    def walk[A1 >: A](fs: (BinZipper[A1] => Option[BinZipper[A1]])*): Option[BinZipper[A1]] =
        fs.foldLeft[Option[BinZipper[A1]]](Some(this))((bz, f) => bz.flatMap(f))

    def goUp = parent.collect {
        case (L, BinZipper(Branch(_, r), ph, pp)) =>
            BinZipper(Branch(bin, r), ph.put(Split(history, Vector())), pp)
        case (R, BinZipper(Branch(l, _), ph, pp)) =>
            BinZipper(Branch(l, bin), ph.put(Split(Vector(), history)), pp)
    }

    def go(side: Side) =
        bin match
            case b @ Branch(_, _) => Some(BinZipper(b(side), Vector(), Some((side, this))))
            case _                => None

    def goLeft  = go(L)
    def goRight = go(R)

    def grow(side: Side) =
        val bin1 = side match
            case L => Branch(Bud, bin)
            case R => Branch(bin, Bud)

        Some(BinZipper(bin1, history.put(Grow(side)), parent))

    def consume(side: Side) =
        bin match
            case Branch(Bud, t) if side == L => Some(BinZipper(t, history.put(Consume(side)), parent))
            case Branch(t, Bud) if side == R => Some(BinZipper(t, history.put(Consume(side)), parent))
            case _                           => None

    def rotate(side: Side) =
        bin match
            case Branch(l, Branch(rl, rr)) if side == L =>
                Some(BinZipper(Branch(Branch(l, rl), rr), history.put(Rotate(L)), parent))
            case Branch(Branch(ll, lr), r) if side == R =>
                Some(BinZipper(Branch(ll, Branch(lr, r)), history.put(Rotate(R)), parent))
            case _                                      => None

    def swap =
        bin match
            case Branch(l, r) => Some(BinZipper(Branch(r, l), history.put(Swap), parent))
            case _            => None

    def modify[A1 >: A](f: A => A1) =
        bin match
            case Leaf(a) => Some(BinZipper(Leaf(f(a)), history, parent))
            case _       => None

    def set[A1 >: A](a: A1) = modify(_ => a)

    def attempt[A1 >: A](f: Option[BinZipper[A1]]): BinZipper[A1] = f.getOrElse(this)

    def clean: BinZipper[A] =
        attempt(walk(_.goLeft, x => Some(x.clean), _.goUp, _.goRight, x => Some(x.clean), _.goUp))
            .attempt(consume(L))
            .attempt(consume(R))

    @tailrec def linearize: BinZipper[A] =
        rotate(R) match
            case Some(t) => t.linearize
            case None    =>
                goRight match
                    case Some(b) => b.linearize
                    case None    => this

    def normalize: BinZipper[A] = clean.linearize.top

    @tailrec def top: BinZipper[A] = goUp match
        case Some(p) => p.top
        case None    => this

    def tree: Bin[A] = top.bin

    def appMany(ops: Vector[BinOp]): Option[BinZipper[A]] = ops.foldOpt(this)(_.app(_))

    def app(op: BinOp): Option[BinZipper[A]] = op match
        case Rotate(side)  => rotate(side)
        case Swap          => swap
        case Split(ls, rs) => walk(_.goLeft, _.appMany(ls), _.goUp, _.goRight, _.appMany(rs), _.goUp)
        case Consume(side) => consume(side)
        case Grow(side)    => grow(side)

    @tailrec def repeat[A1 >: A](count: Int)(act: BinZipper[A1] => Option[BinZipper[A1]]): Option[BinZipper[A1]] =
        if count == 0 then Some(this)
        else
            act(this) match
                case Some(z) => z.repeat(count - 1)(act)
                case None    => None

    def swapNext = rotate(L) match
        case Some(z) => z.walk(_.goLeft, _.swap, _.goUp, _.rotate(R))
        case None    => swap

    def swapElems(i: Int, j: Int) =
        if (i >= j) Some(this)
        else
            walk(
              _.repeat(i)(_.goRight),
              _.repeat(j - i - 1)(_.walk(_.swapNext, _.goRight)),
              _.swapNext,
              _.repeat(j - i - 1)(_.walk(_.goUp, _.swapNext)),
              _.repeat(i)(_.goUp)
            )
end BinZipper

enum BinHistory[+A] derives Traverse:
    case HRotate(side: Side, l: A, m: A, r: A)
    case HSwap(l: A, r: A)
    case HSplit(left: HChain[A], right: HChain[A])
    case HConsume(side: Side, v: A)
    case HGrow(side: Side, v: A)

    final def op: BinOp = this match
        case HRotate(side, _, _, _) => Rotate(side)
        case HSwap(_, _)            => Swap
        case HSplit(l, r)           => Split(l.history.map(_.op), r.history.map(_.op))
        case HConsume(side, _)      => Consume(side)
        case HGrow(side, _)         => Grow(side)
end BinHistory

object BinHistory:
    final case class HChain[+A](start: A, end: A, history: Vector[BinHistory[A]]) derives Traverse

final type Adaptation[+A] = Vector[BinHistory[A]]
trait EmptyHistory:
    def history: Adaptation[Nothing] = Vector.empty

enum BinRes[+A]:
    case LeafRes[+A](value: A) extends BinRes[A], EmptyHistory
    case BranchRes(l: BinRes[A], r: BinRes[A], value: A, history: Adaptation[A])
    case FailRes(value: A, history: Adaptation[A], last: BinOp)

    import BinRes.{FailRes, LeafRes, BranchRes}
    def value: A

    inline def ifBranch[A1 >: A](op: BinOp)(inline f: (BinRes[A], BinRes[A]) => BinRes[A1]): BinRes[A1] =
        this match
            case LeafRes(value)                  => FailRes(value, history, op)
            case BranchRes(l, r, value, history) => f(l, r)
            case FailRes(value, history, last)   => this

    def history: Adaptation[A]

    final def clean: BinRes[A] = this match
        case fail: FailRes[?]     => fail.copy(history = Vector())
        case branch: BranchRes[?] => branch.copy(history = Vector())
        case (_: LeafRes[?])      => this

    final def withHistory[A1 >: A](prev: BinRes[A1])(op: BinHistory[A1]): BinRes[A1] = this match
        case branch: BranchRes[?]         => branch.copy(history = prev.history :+ op)
        case _: (LeafRes[?] | FailRes[?]) => this

    def success: Boolean = this match
        case _: (LeafRes[?] | BranchRes[?]) => true
        case f: FailRes[?]                  => false
end BinRes

object BinRes:

    def apply[A](bin: Bin[A])(using A: PMagma[A]): BinRes[A] = bin match
        case Leaf(a)      => LeafRes(a)
        case Branch(l, r) => BinRes(l).branch(BinRes(r))
        case Bud          => LeafRes(A.empty)

    extension [A](b: BinRes[A])(using A: PMagma[A])
        def branch(c: BinRes[A]): BinRes[A] = c match
            case FailRes(v, h, ls) => FailRes(b.value ## c.value, h, ls)
            case _                 => BranchRes(b, c, b.value ## c.value, Vector())

        def modAll(ops: Vector[BinOp]): BinRes[A] = ops.foldLeft(b)(_.mod(_))

        def mod(op: BinOp): BinRes[A] =
            op match
                case Grow(L) => LeafRes(A.empty).branch(b).withHistory(b)(HGrow(L, b.value))
                case Grow(R) => b.branch(LeafRes(A.empty)).withHistory(b)(HGrow(R, b.value))
                case _       =>
                    b.ifBranch(op) { (l, r) =>
                        op match
                            case Swap      => (r.branch(l)).withHistory(b)(HSwap(l.value, r.value))
                            case Rotate(R) =>
                                l.ifBranch(op): (ll, lr) =>
                                    ll.branch(lr.branch(r)).withHistory(b)(HRotate(R, ll.value, lr.value, r.value))

                            case Rotate(L) =>
                                r.ifBranch(op): (rl, rr) =>
                                    l.branch(rl).branch(rr).withHistory(b)(HRotate(L, l.value, rl.value, rr.value))

                            case Split(ls, rs) =>
                                val lt = l.clean.modAll(ls)
                                val rt = r.clean.modAll(rs)
                                lt.clean
                                    .branch(rt.clean)
                                    .withHistory(b)(
                                      HSplit(
                                        HChain(l.value, lt.value, lt.history),
                                        HChain(r.value, rt.value, rt.history)
                                      )
                                    )

                            case Consume(L) => r.withHistory(b)(HConsume(L, r.value))
                            case Consume(R) => l.withHistory(b)(HConsume(R, r.value))
                            case Grow(_)    => b
                        end match
                    }
            end match
        end mod
    end extension

end BinRes
