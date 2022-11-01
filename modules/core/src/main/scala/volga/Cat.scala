package volga

import volga.Cat.IsoImpl
import volga.tags.Tensor
import volga.tags.One
import volga.tags.Closure
import volga.tags.Hom
import volga.tags.Plus

transparent trait Aliases[U[_]]:
  infix type -->[A, B]  = U[tags.Hom[A, B]]
  infix type <--[A, B]  = B --> A
  type Ob[A]            = U[tags.Obj[A]]
  type I                = U[tags.One]
  infix type x[A, B]    = U[tags.Tensor[A, B]]
  infix type <-->[A, B] = Iso[U, A, B]
  infix type ==>[A, B]  = U[tags.Closure[A, B]]
  type ^[A]             = U[tags.Dual[A]]
  infix type +[A, B]    = U[tags.Plus[A, B]]
  type O                = U[tags.Zero]

trait Iso[U[_], A, B] extends Aliases[U]:
  self =>
  def to: A --> B
  def from: B --> A

  def compose[C](other: C <--> A)(using Cat[U], Ob[A], Ob[B], Ob[C]): C <--> B = new:
    val to   = self.to <<< other.to
    val from = self.from >>> other.from

  def andThen[C](other: B <--> C)(using Cat[U], Ob[A], Ob[B], Ob[C]): A <--> C = other compose self

  def inverse: B <--> A = new:
    val from             = self.to
    val to               = self.from
    override def inverse = self

  def tensor[C, D](other: C <--> D)(using MonoidalCat[U], Ob[A], Ob[B], Ob[C], Ob[D]): (A x C) <--> (B x D) =
    Iso(self.to >< other.to, self.from >< other.from)

object Iso:
  def apply[U[_], A, B](to: U[tags.Hom[A, B]], from: U[tags.Hom[B, A]]): Iso[U, A, B] =
    val f = to
    val g = from
    new:
      val to   = f
      val from = g

  def inv[U[_], A, B](f: Iso[U, A, B]): Iso[U, B, A] = f.inverse

type IsoCat[U[_], A] = A match
  case tags.Obj[a]       => U[tags.Obj[a]]
  case tags.Hom[a, b]    => Iso[U, a, b]
  case tags.One          => U[tags.One]
  case tags.Tensor[a, b] => U[tags.Tensor[a, b]]

trait Cat[U[_]] extends Aliases[U]:
  self =>
  def identity[A: Ob]: A --> A
  def compose[A: Ob, B: Ob, C: Ob](f: B --> C, g: A --> B): A --> C

  protected given this.type = self

  extension [A: Ob, B: Ob](f: A --> B)
    infix def >>>[C: Ob](g: B --> C): A --> C = compose(g, f)
    infix def <<<[C: Ob](g: C --> A): C --> B = compose(f, g)

  given isoCat: Cat[[A] =>> IsoCat[U, A]] = new IsoImpl[U](using this)

  def idIso[A: Ob]: A <--> A = Iso(identity, identity)

object Cat:
  open class IsoImpl[U[_]](using U: Cat[U]) extends Cat[[A] =>> IsoCat[U, A]]:
    def identity[A: Ob]: Iso[U, A, A]                                 = new:
      val from = U.identity
      val to   = U.identity
    def compose[A: Ob, B: Ob, C: Ob](f: B --> C, g: A --> B): A --> C =
      Iso(to = f.to <<< g.to, from = f.from >>> g.from)

trait MonoidalCat[U[_]] extends Cat[U]:
  given unitOb: Ob[I]
  given tensorOb[A: Ob, B: Ob]: Ob[A x B]

  def tensor[A: Ob, B: Ob, C: Ob, D: Ob](
      f: A --> B,
      g: C --> D
  ): (A x C) --> (B x D)

  def associate[A: Ob, B: Ob, C: Ob]: (A x (B x C)) <--> ((A x B) x C)
  def leftUnit[A: Ob]: (A x I) <--> A
  def rightUnit[A: Ob]: (I x A) <--> A

  extension [A: Ob, B: Ob](f: A --> B) infix def ><[C: Ob, D: Ob](g: C --> D): (A x C) --> (B x D) = tensor(f, g)

trait SymmetricCat[U[_]] extends MonoidalCat[U]:
  def braiding[A: Ob, B: Ob]: (A x B) --> (B x A)

  def symmetry[A: Ob, B: Ob]: (A x B) <--> (B x A) = Iso(braiding, braiding)

  override def rightUnit[A: Ob] = leftUnit[A].compose(symmetry[I, A])(using this)

trait CartesianCat[U[_]] extends SymmetricCat[U]:
  def terminal[A: Ob]: A --> I

  def projectLeft[A: Ob, B: Ob]: (A x B) --> A
  def projectRight[A: Ob, B: Ob]: (A x B) --> B
  def product[A: Ob, B: Ob, C: Ob](f: A --> B, g: A --> C): A --> (B x C)

  final def pl[A: Ob, B: Ob]: (A x B) --> A = projectLeft
  final def pr[A: Ob, B: Ob]: (A x B) --> B = projectRight

  override def braiding[A: Ob, B: Ob]: (A x B) --> (B x A) = product(projectRight, projectLeft)

  override def leftUnit[A: Ob]: (A x I) <--> A  = Iso(projectLeft, product(identity, terminal))
  override def rightUnit[A: Ob]: (I x A) <--> A = Iso(projectRight, product(terminal, identity))

trait CocartesianCat[U[_]] extends Cat[U]:
  given zeroOb: Ob[O]
  given sumOb[A: Ob, B: Ob]: Ob[A + B]

  def initial[A: Ob]: O --> A

  def injectLeft[A: Ob, B: Ob]: A --> (A + B)
  def injectRight[A: Ob, B: Ob]: B --> (A + B)

  def sum[A: Ob, B: Ob, C: Ob](f: A --> C, g: B --> C): (A + B) --> C

trait DistributiveCat[U[_]] extends CartesianCat[U] with CocartesianCat[U]:
  def distributeFrom[A: Ob, B: Ob, C: Ob]: (A x (B + C)) --> ((A x B) + (A x C))

  def distributeTo[A: Ob, B: Ob, C: Ob]: ((A x B) + (A x C)) --> (A x (B + C)) =
    sum(
      tensor(identity[A], injectLeft),
      tensor(identity[A], injectRight)
    )

  def distribute[A: Ob, B: Ob, C: Ob]: ((A x B) + (A x C)) <--> (A x (B + C)) = Iso(distributeTo, distributeFrom)

trait ClosedCat[U[_]] extends SymmetricCat[U]:
  given closureOb[A: Ob, B: Ob]: Ob[A ==> B]

  def closureMap[A: Ob, B: Ob, C: Ob](f: A --> B): (C ==> A) --> (C ==> B)
  def curry[A: Ob, B: Ob, C: Ob](f: (A x B) --> C): B --> (A ==> C)
  def uncurry[A: Ob, B: Ob, C: Ob](f: B --> (A ==> C)): (A x B) --> C

  def closureUnit[A: Ob, B: Ob]: A --> (B ==> (B x A))   = curry(identity)
  def closureCounit[A: Ob, B: Ob]: (A x (A ==> B)) --> B = uncurry(identity)
  def application[A: Ob, B: Ob]: (A x (A ==> B)) --> B   = closureCounit

  def curryRight[A: Ob, B: Ob, C: Ob](f: (B x A) --> C): B --> (A ==> C)   = curry(f <<< braiding)
  def uncurryRight[A: Ob, B: Ob, C: Ob](f: B --> (A ==> C)): (B x A) --> C = uncurry(f) <<< braiding

trait ClosedCartesianCat[U[_]] extends ClosedCat[U] with CartesianCat[U]

trait ClosedDistributiveCat[U[_]] extends ClosedCartesianCat[U] with DistributiveCat[U]:
  def distributeFrom[A: Ob, B: Ob, C: Ob]: (A x (B + C)) --> ((A x B) + (A x C)) =
    uncurry(sum(curry(injectLeft), curry(injectRight)))

trait CompactCat[U[_]] extends ClosedCat[U]:
  given dual[A: Ob]: Ob[^[A]]

  def duality[A: Ob]: I <--> (A x ^[A])

  given dualClosureEquality[A: Ob, B: Ob]: ((A ==> B) =:= (^[A] x B))

  def rightDuality[A: Ob]: I <--> (^[A] x A) = duality andThen symmetry

  def doubleDual[A: Ob]: ^[^[A]] <--> A =
    leftUnit compose                                /// A x I
      (idIso tensor Iso.inv(duality[^[A]])) compose /// A x (^A  x ^^A)
      Iso.inv(associate) compose                    ///  (A x ^A) x ^^A
      (duality tensor idIso) compose                /// I x ^^A
      Iso.inv(rightUnit)                            /// ^^A

  def substClosure[F[_], A: Ob, B: Ob](fd: F[^[A] x B]): F[A ==> B] =
    dualClosureEquality.substituteContra(fd)

  def substClosureBack[F[_], A: Ob, B: Ob](fd: F[A ==> B]): F[^[A] x B] =
    dualClosureEquality.substituteCo(fd)

  override given closureOb[A: Ob, B: Ob]: Ob[A ==> B] = substClosure(tensorOb)

  override def curry[A: Ob, B: Ob, C: Ob](f: (A x B) --> C): B --> (A ==> C) =
    substClosure[[x] =>> B --> x, A, C](
      rightUnit[B].from >>>                     /// I x B
        (rightDuality[A].to >< identity[B]) >>> /// (^A x A) x B
        associate.from >>> (                    /// ^A x (A x B)
          identity[^[A]] >< f                   /// ^A x C
        )
    )

  override def uncurry[A: Ob, B: Ob, C: Ob](f: B --> (A ==> C)): (A x B) --> C =
    (identity[A] >< substClosureBack[[x] =>> B --> x, A, C](f)) >>> /// A x (^A x C)
      associate.to >>>                                              /// (A x ^A) x C
      (duality[A].from >< identity) >>>                             /// I x C
      rightUnit.to
