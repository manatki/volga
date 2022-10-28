package volga
import cats.arrow.{Arrow, ArrowChoice}
import simulacrum.{op, typeclass}

@typeclass
trait ArrLike[->[_, _]]

object ArrLike extends ArrInstanceChain[ArrLike]
@typeclass
trait Arr[->[_, _]] extends Cat[->] with ArrLike[->] {
  def lift[A, B](f: A => B): A -> B
  @op("***", alias = true)
  def split[A, B, C, D](f: A -> C, g: B -> D): (A, B) -> (C, D)

  def proj1[A, B]: (A, B) -> A = lift(_._1)
  def proj2[A, B]: (A, B) -> B = lift(_._2)

  @op("&&&", alias = true)
  def product[A, B, C](f: A -> B, g: A -> C): A -> (B, C) = compose(split(f, g), lift(a => (a, a)))

  def term[A]: A -> Unit = lift(_ => ())
  def id[A]: A -> A      = lift(identity)

  def rmap[A, B, C](a1: A -> B)(f: B => C): A -> C = compose(lift(f), a1)
  def lmap[A, B, C](a1: A -> B)(f: C => A): C -> B = compose(a1, lift(f))

  def first[A, B, C](f: A -> B): (A, C) -> (B, C)  = split(f, id)
  def second[A, B, C](f: A -> B): (C, A) -> (C, B) = split(id, f)

  def product3[A, B1, B2, B3](a1: A -> B1, a2: A -> B2, a3: A -> B3): A -> (B1, B2, B3) =
    rmap(product(product(a1, a2), a3)) { case ((b1, b2), b3) => (b1, b2, b3) }
  def product4[A, B1, B2, B3, B4](a1: A -> B1, a2: A -> B2, a3: A -> B3, a4: A -> B4): A -> (B1, B2, B3, B4) =
    rmap(product(product(a1, a2), product(a3, a4))) { case ((b1, b2), (b3, b4)) => (b1, b2, b3, b4) }
  def product5[A, B1, B2, B3, B4, B5](
      a1: A -> B1,
      a2: A -> B2,
      a3: A -> B3,
      a4: A -> B4,
      a5: A -> B5
  ): A -> (B1, B2, B3, B4, B5) =
    rmap(product(product(a1, a2), product(product(a3, a4), a5))) {
      case ((b1, b2), ((b3, b4), b5)) => (b1, b2, b3, b4, b5)
    }

  def mergeMap2[A, B1, B2, C](a1: A -> B1, a2: A -> B2)(f: (B1, B2) => C) =
    rmap(product(a1, a2)) { case (b1, b2) => f(b1, b2) }
  def mergeMap3[A, B1, B2, B3, C](a1: A -> B1, a2: A -> B2, a3: A -> B3)(f: (B1, B2, B3) => C) =
    rmap(product(product(a1, a2), a3)) { case ((b1, b2), b3) => f(b1, b2, b3) }
  def mergeMap4[A, B1, B2, B3, B4, C](a1: A -> B1, a2: A -> B2, a3: A -> B3, a4: A -> B4)(f: (B1, B2, B3, B4) => C) =
    rmap(product(product(a1, a2), product(a3, a4))) { case ((b1, b2), (b3, b4)) => f(b1, b2, b3, b4) }
  def mergeMap5[A, B1, B2, B3, B4, B5, C](a1: A -> B1, a2: A -> B2, a3: A -> B3, a4: A -> B4, a5: A -> B5)(
      f: (B1, B2, B3, B4, B5) => C
  ) =
    rmap(product(product(product(a1, a2), product(a3, a4)), a5)) {
      case (((b1, b2), (b3, b4)), b5) => f(b1, b2, b3, b4, b5)
    }
}

object Arr extends ArrInstanceChain[Arr]

@typeclass
trait ArrChoice[->[_, _]] extends Arr[->] {
  @op("+++", alias = true)
  def choose[A, B, C, D](f: A -> C)(g: B -> D): Either[A, B] -> Either[C, D]

  @op("|||", alias = true)
  def choice[A, B, C](f: A -> C)(g: B -> C): Either[A, B] -> C =
    andThen(choose(f)(g))(lift(_.merge))

  def left[A, B, C](fab: A -> B): Either[A, C] -> Either[B, C] =
    choose(fab)(lift(identity[C]))

  def right[A, B, C](fab: A -> B): Either[C, A] -> Either[C, B] =
    choose(lift(identity[C]))(fab)
}

object ArrChoice extends ArrChoiceInstanceChain[ArrChoice]

@typeclass trait ArrPlus[->[_, _]] extends Arr[->] {
  @op(">+<", alias = true)
  def plus[A, B](f: A -> B, g: A -> B): A -> B
}

@typeclass trait ArrApply[->[_, _]] extends ArrChoice[->] {
  def app[A, B]: (A, A -> B) -> B

  override def choose[A, B, C, D](f: A -> C)(g: B -> D): Either[A, B] -> Either[C, D] =
    andThen(lift[Either[A, B], (Unit, Unit -> Either[C, D])] {
      case Left(a)  => ((), rmap[Unit, C, Either[C, D]](lmap[A, C, Unit](f)(_ => a))(Left(_)))
      case Right(b) => ((), rmap[Unit, D, Either[C, D]](lmap[B, D, Unit](g)(_ => b))(Right(_)))
    })(app)
}

trait ArrInstanceChain[TC[a[_, _]] >: Arr[a]] {
  implicit def arrowFromCats[->[_, _]](implicit arr: Arrow[->]): TC[->] =
    new Arr[->] {
      def lift[A, B](f: A => B): A -> B                             = arr.lift(f)
      def split[A, B, C, D](f: A -> C, g: B -> D): (A, B) -> (C, D) = arr.split(f, g)
      def compose[A, B, C](f: B -> C, g: A -> B): A -> C            = arr.compose(f, g)
    }
}

trait ArrChoiceInstanceChain[TC[a[_, _]] >: ArrChoice[a]] {
  implicit def arrowChoiceFromCats[->[_, _]](implicit arr: ArrowChoice[->]): TC[->] =
    new ArrChoice[->] {
      def lift[A, B](f: A => B): A -> B                                          = arr.lift(f)
      def split[A, B, C, D](f: A -> C, g: B -> D): (A, B) -> (C, D)              = arr.split(f, g)
      def compose[A, B, C](f: B -> C, g: A -> B): A -> C                         = arr.compose(f, g)
      def choose[A, B, C, D](f: A -> C)(g: B -> D): Either[A, B] -> Either[C, D] = arr.choose(f)(g)
    }
}
