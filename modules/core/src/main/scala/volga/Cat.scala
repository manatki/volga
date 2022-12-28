package volga

import volga.Cat.IsoImpl
import volga.tags.*

transparent trait HomAliases[H[_, _]]:
  infix type -->[A, B] = H[A, B]
  infix type <--[A, B] = H[B, A]

transparent trait ObAliases[U[_]]:

  type Ob[A]           = U[tags.Obj[A]]
  type I               = U[tags.One]
  infix type x[A, B]   = U[tags.Tensor[A, B]]
  infix type ==>[A, B] = U[tags.Closure[A, B]]
  type ^[A]            = U[tags.Dual[A]]
  infix type +[A, B]   = U[tags.Plus[A, B]]
  type O               = U[tags.Zero]
  type $[A]            = U[tags.Scala[A]]

transparent trait Aliases[H[_, _], U[_]] extends HomAliases[H] with ObAliases[U]:
  infix type <-->[A, B] = Iso[H, U, A, B]

trait Iso[H[_, _], U[_], A, B] extends Aliases[H, U]:
  self =>
  def to: H[A, B]
  def from: H[B, A]

  def compose[C](other: C <--> A)(using Cat[H, U], Ob[A], Ob[B], Ob[C]): C <--> B = new:
    val to   = self.to <<< other.to
    val from = self.from >>> other.from

  def andThen[C](other: B <--> C)(using Cat[H, U], Ob[A], Ob[B], Ob[C]): A <--> C =
    other compose self

  def inverse: B <--> A = new:
    val from             = self.to
    val to               = self.from
    override def inverse = self

  def tensor[C, D](
      other: Iso[H, U, C, D]
  )(using MonoidalCat[H, U], Ob[A], Ob[B], Ob[C], Ob[D]): (A x C) <--> (B x D) =
    Iso(self.to >< other.to, self.from >< other.from)

object Iso:
  def apply[H[_, _], U[_], A, B](to: H[A, B], from: H[B, A]): Iso[H, U, A, B] =
    val f = to
    val g = from
    new:
      val to   = f
      val from = g

  def inv[H[_, _], U[_], A, B](f: Iso[H, U, A, B]): Iso[H, U, B, A] = f.inverse

trait Cat[H[_, _], U[_]] extends Aliases[H, U]:
  self =>
  def identity[A: Ob]: A --> A
  def compose[A: Ob, B: Ob, C: Ob](f: B --> C, g: A --> B): A --> C

  protected given this.type = self

  extension [A: Ob, B: Ob](f: A --> B)
    infix def >>>[C: Ob](g: B --> C): A --> C = compose(g, f)
    infix def <<<[C: Ob](g: C --> A): C --> B = compose(f, g)

  given isoCat: Cat[[x, y] =>> Iso[H, U, x, y], U] = new IsoImpl[H, U](using this)

  def idIso[A: Ob]: A <--> A = Iso(identity, identity)

object Cat:
  open class IsoImpl[H[_, _], U[_]](using U: Cat[H, U]) extends Cat[[x, y] =>> Iso[H, U, x, y], U]:
    def identity[A: Ob]: Iso[H, U, A, A]                                 = new:
      val from = U.identity
      val to   = U.identity
    def compose[A: Ob, B: Ob, C: Ob](f: B --> C, g: A --> B): A --> C =
      Iso(to = f.to <<< g.to, from = f.from >>> g.from)

trait MonoidalCat[H[_, _], U[_]] extends Cat[H, U]:
  given unitOb: Ob[I]
  given tensorOb[A: Ob, B: Ob]: Ob[A x B]

  def tensor[A: Ob, B: Ob, C: Ob, D: Ob](f: A --> B, g: C --> D): (A x C) --> (B x D)

  def associate[A: Ob, B: Ob, C: Ob]: (A x (B x C)) <--> ((A x B) x C)
  def rightUnit[A: Ob]: (A x I) <--> A
  def leftUnit[A: Ob]: (I x A) <--> A

  extension [A: Ob, B: Ob](f: A --> B) infix def ><[C: Ob, D: Ob](g: C --> D): (A x C) --> (B x D) = tensor(f, g)

trait SymmetricCat[H[_, _], U[_]] extends MonoidalCat[H, U]:
  def braiding[A: Ob, B: Ob]: (A x B) --> (B x A)

  def symmetry[A: Ob, B: Ob]: (A x B) <--> (B x A) = Iso(braiding, braiding)

  def assocLeft[A: Ob, B: Ob, C: Ob]: (A x (B x C)) --> ((A x B) x C)

  override def rightUnit[A: Ob] = leftUnit[A].compose(symmetry[A, I])(using this)

  override def associate[A: Ob, B: Ob, C: Ob]: (A x (B x C)) <--> ((A x B) x C) = Iso(
    assocLeft,
    braiding[A x B, C] >>>
      (identity[C] >< braiding) >>>
      assocLeft >>> braiding >>>
      (identity[A] >< braiding)
  )

trait CartesianCat[H[_, _], U[_]] extends SymmetricCat[H, U]:
  def terminal[A: Ob]: A --> I

  def projectLeft[A: Ob, B: Ob]: (A x B) --> A
  def projectRight[A: Ob, B: Ob]: (A x B) --> B

  def product[A: Ob, B: Ob, C: Ob](f: A --> B, g: A --> C): A --> (B x C)

  final def pl[A: Ob, B: Ob]: (A x B) --> A = projectLeft
  final def pr[A: Ob, B: Ob]: (A x B) --> B = projectRight

  override def braiding[A: Ob, B: Ob]: (A x B) --> (B x A) = product(projectRight, projectLeft)

  override def rightUnit[A: Ob]: (A x I) <--> A = Iso(projectLeft, product(identity, terminal))
  override def leftUnit[A: Ob]: (I x A) <--> A  = Iso(projectRight, product(terminal, identity))

trait CocartesianCat[H[_, _], U[_]] extends Cat[H, U]:
  given zeroOb: Ob[O]
  given sumOb[A: Ob, B: Ob]: Ob[A + B]

  def initial[A: Ob]: O --> A

  def injectLeft[A: Ob, B: Ob]: A --> (A + B)
  def injectRight[A: Ob, B: Ob]: B --> (A + B)

  def sum[A: Ob, B: Ob, C: Ob](f: A --> C, g: B --> C): (A + B) --> C

trait DistributiveCat[H[_, _], U[_]] extends CartesianCat[H, U] with CocartesianCat[H, U]:
  def distributeFrom[A: Ob, B: Ob, C: Ob]: (A x (B + C)) --> ((A x B) + (A x C))

  def distributeTo[A: Ob, B: Ob, C: Ob]: ((A x B) + (A x C)) --> (A x (B + C)) =
    sum(
      tensor(identity[A], injectLeft),
      tensor(identity[A], injectRight)
    )

  def distribute[A: Ob, B: Ob, C: Ob]: ((A x B) + (A x C)) <--> (A x (B + C)) = Iso(distributeTo, distributeFrom)

trait ClosedCat[H[_, _], U[_]] extends SymmetricCat[H, U]:
  given closureOb[A: Ob, B: Ob]: Ob[A ==> B]

  def closureMap[A: Ob, B: Ob, C: Ob](f: A --> B): (C ==> A) --> (C ==> B)
  def curry[A: Ob, B: Ob, C: Ob](f: (A x B) --> C): B --> (A ==> C)
  def uncurry[A: Ob, B: Ob, C: Ob](f: B --> (A ==> C)): (A x B) --> C

  def closureUnit[A: Ob, B: Ob]: A --> (B ==> (B x A))   = curry(identity)
  def closureCounit[A: Ob, B: Ob]: (A x (A ==> B)) --> B = uncurry(identity)
  def application[A: Ob, B: Ob]: (A x (A ==> B)) --> B   = closureCounit

  def curryRight[A: Ob, B: Ob, C: Ob](f: (B x A) --> C): B --> (A ==> C)   = curry(f <<< braiding)
  def uncurryRight[A: Ob, B: Ob, C: Ob](f: B --> (A ==> C)): (B x A) --> C = uncurry(f) <<< braiding

trait ClosedCartesianCat[H[_, _], U[_]] extends ClosedCat[H, U] with CartesianCat[H, U]

trait ClosedDistributiveCat[H[_, _], U[_]] extends ClosedCartesianCat[H, U] with DistributiveCat[H, U]:
  def distributeFrom[A: Ob, B: Ob, C: Ob]: (A x (B + C)) --> ((A x B) + (A x C)) =
    uncurry(sum(curry(injectLeft), curry(injectRight)))

trait CompactCat[H[_, _], U[_]] extends ClosedCat[H, U]:
  given dual[A: Ob]: Ob[^[A]]

  def duality[A: Ob]: I <--> (A x ^[A])

  given dualClosureEquality[A: Ob, B: Ob]: ((A ==> B) =:= (^[A] x B))

  def rightDuality[A: Ob]: I <--> (^[A] x A) = duality andThen symmetry

  def doubleDual[A: Ob]: ^[^[A]] <--> A =
    rightUnit compose                               /// A x I
      (idIso tensor Iso.inv(duality[^[A]])) compose /// A x (^A  x ^^A)
      Iso.inv(associate) compose                    ///  (A x ^A) x ^^A
      (duality tensor idIso) compose                /// I x ^^A
      Iso.inv(leftUnit)                             /// ^^A

  def substClosure[F[_], A: Ob, B: Ob](fd: F[^[A] x B]): F[A ==> B] =
    dualClosureEquality.substituteContra(fd)

  def substClosureBack[F[_], A: Ob, B: Ob](fd: F[A ==> B]): F[^[A] x B] =
    dualClosureEquality.substituteCo(fd)

  override given closureOb[A: Ob, B: Ob]: Ob[A ==> B] = substClosure(tensorOb)

  override def curry[A: Ob, B: Ob, C: Ob](f: (A x B) --> C): B --> (A ==> C) =
    substClosure[[x] =>> B --> x, A, C](
      leftUnit[B].from >>>                      /// I x B
        (rightDuality[A].to >< identity[B]) >>> /// (^A x A) x B
        associate.from >>> (                    /// ^A x (A x B)
          identity[^[A]] >< f                   /// ^A x C
        )
    )

  override def uncurry[A: Ob, B: Ob, C: Ob](f: B --> (A ==> C)): (A x B) --> C =
    (identity[A] >< substClosureBack[[x] =>> B --> x, A, C](f)) >>> /// A x (^A x C)
      associate.to >>>                                              /// (A x ^A) x C
      (duality[A].from >< identity) >>>                             /// I x C
      leftUnit.to

trait HasScalaFunctor[H[_, _], U[_]] extends Aliases[H, U]:
  def lift[A, B](f: A => B): $[A] --> $[B]
