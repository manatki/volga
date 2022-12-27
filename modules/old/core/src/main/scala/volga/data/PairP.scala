package volga
package data

final case class PairP[P[_, _], Q[_, _], A, B](pab: P[A, B], qab: Q[A, B])

object PairP extends PairPInstances1

private[volga] trait PairPCatInstance[P[_, _], Q[_, _]] extends Cat[PairP[P, Q, *, *]] {
  protected[this] val P: Cat[P]
  protected[this] val Q: Cat[Q]

  final override def id[A]: PairP[P, Q, A, A] = PairP(P.id, Q.id)

  final override def compose[A, B, C](f: PairP[P, Q, B, C], g: PairP[P, Q, A, B]): PairP[P, Q, A, C] =
    PairP(P.compose(f.pab, g.pab), Q.compose(f.qab, g.qab))
}

private[volga] trait PairPArrInstance[P[_, _], Q[_, _]] extends Arr[PairP[P, Q, *, *]] with PairPCatInstance[P, Q] {
  override protected[this] val P: Arr[P]
  override protected[this] val Q: Arr[Q]

  final override def lift[A, B](f: A => B): PairP[P, Q, A, B] =
    PairP(P.lift(f), Q.lift(f))
  final override def split[A, B, C, D](f: PairP[P, Q, A, B], g: PairP[P, Q, C, D]): PairP[P, Q, (A, C), (B, D)] =
    PairP(P.split(f.pab, g.pab), Q.split(f.qab, g.qab))
}

private[volga] trait PairPArrChoiceInstance[P[_, _], Q[_, _]]
    extends ArrChoice[PairP[P, Q, *, *]] with PairPArrInstance[P, Q] {
  override protected[this] val P: ArrChoice[P]
  override protected[this] val Q: ArrChoice[Q]

  final override def choose[A, B, C, D](
      f: PairP[P, Q, A, C]
  )(g: PairP[P, Q, B, D]): PairP[P, Q, Either[A, B], Either[C, D]] =
    PairP(P.choose(f.pab)(g.pab), Q.choose(f.qab)(g.qab))

  final override def choice[A, B, C](f: PairP[P, Q, A, C])(g: PairP[P, Q, B, C]): PairP[P, Q, Either[A, B], C] =
    PairP(P.choice(f.pab)(g.pab), Q.choice(f.qab)(g.qab))
}

private[volga] trait PairPSemigroupalCatInstance[P[_, _], Q[_, _], x[_, _]]
    extends SemigropalCat[PairP[P, Q, *, *], x] with PairPCatInstance[P, Q] {
  override protected[this] val P: SemigropalCat[P, x]
  override protected[this] val Q: SemigropalCat[Q, x]
  final override def assocl[A, B, C] = PairP(P.assocl, Q.assocl)
  final override def assocr[A, B, C] = PairP(P.assocr, Q.assocr)
  final override def tensor[A, B, C, D](f: PairP[P, Q, A, B], g: PairP[P, Q, C, D]): PairP[P, Q, x[A, C], x[B, D]] =
    PairP(P.tensor(f.pab, g.pab), Q.tensor(f.qab, g.qab))
}

private[volga] trait PairPMonoidalCatInstance[P[_, _], Q[_, _], x[_, _], I]
    extends MonoidalCat[PairP[P, Q, *, *], x, I] with PairPSemigroupalCatInstance[P, Q, x] {
  override protected[this] val P: MonoidalCat[P, x, I]
  override protected[this] val Q: MonoidalCat[Q, x, I]
  final override def lunit[A]: PairP[P, Q, x[I, A], A] = PairP(P.lunit, Q.lunit)
  final override def unitl[A]: PairP[P, Q, A, x[I, A]] = PairP(P.unitl, Q.unitl)
  final override def runit[A]: PairP[P, Q, x[A, I], A] = PairP(P.runit, Q.runit)
  final override def unitr[A]: PairP[P, Q, A, x[A, I]] = PairP(P.unitr, Q.unitr)
}

private[volga] trait PairPSymMonoidalCatInstance[P[_, _], Q[_, _], x[_, _], I]
    extends Symon[PairP[P, Q, *, *], x, I] with PairPMonoidalCatInstance[P, Q, x, I] {
  override protected[this] val P: Symon[P, x, I]
  override protected[this] val Q: Symon[Q, x, I]

  def swap[A, B]: PairP[P, Q, x[A, B], x[B, A]] = PairP(P.swap, Q.swap)
}

private[volga] class PairPCatInstanceC[P[_, _], Q[_, _]](
    implicit protected[this] val P: Cat[P],
    protected[this] val Q: Cat[Q]
) extends PairPCatInstance[P, Q]

private[volga] class PairPArrInstanceC[P[_, _], Q[_, _]](
    implicit protected[this] val P: Arr[P],
    protected[this] val Q: Arr[Q]
) extends PairPArrInstance[P, Q]

private[volga] class PairPArrChoiceInstanceC[P[_, _], Q[_, _]](
    implicit protected[this] val P: ArrChoice[P],
    protected[this] val Q: ArrChoice[Q]
) extends PairPArrChoiceInstance[P, Q]

private[volga] class PairPSemigroupalCatInstanceC[P[_, _], Q[_, _], x[_, _]](
    implicit protected[this] val P: SemigropalCat[P, x],
    protected[this] val Q: SemigropalCat[Q, x]
) extends PairPSemigroupalCatInstance[P, Q, x]

private[volga] class PairPMonoidalCatInstanceC[P[_, _], Q[_, _], x[_, _], I](
    implicit protected[this] val P: MonoidalCat[P, x, I],
    protected[this] val Q: MonoidalCat[Q, x, I]
) extends PairPMonoidalCatInstance[P, Q, x, I]

private[volga] class PairPSymMonoidalCatInstanceC[P[_, _], Q[_, _], x[_, _], I](
    implicit protected[this] val P: Symon[P, x, I],
    protected[this] val Q: Symon[Q, x, I]
) extends PairPSymMonoidalCatInstance[P, Q, x, I]

trait PairPInstances1 extends PairPInstances2 {
  implicit def symMonPairP[P[_, _], Q[_, _], x[_, _], I](
      implicit P: Symon[P, x, I],
      Q: Symon[Q, x, I]
  ): Symon[PairP[P, Q, *, *], x, I] =
    new PairPSymMonoidalCatInstanceC

  implicit def arrChoicePairP[P[_, _], Q[_, _]](
      implicit P: ArrChoice[P],
      Q: ArrChoice[Q]
  ): ArrChoice[PairP[P, Q, *, *]] =
    new PairPArrChoiceInstanceC
}

trait PairPInstances2 extends PairPInstances3 {
  implicit def monCatPairP[P[_, _], Q[_, _], x[_, _], I](
      implicit P: MonoidalCat[P, x, I],
      Q: MonoidalCat[Q, x, I]
  ): MonoidalCat[PairP[P, Q, *, *], x, I] =
    new PairPMonoidalCatInstanceC

  implicit def arrPairP[P[_, _], Q[_, _]](implicit P: Arr[P], Q: Arr[Q]): Arr[PairP[P, Q, *, *]] =
    new PairPArrInstanceC
}

trait PairPInstances3 extends PairPInstances4 {
  implicit def semigroupalPairP[P[_, _], Q[_, _], x[_, _]](
      implicit P: SemigropalCat[P, x],
      Q: SemigropalCat[Q, x]
  ): SemigropalCat[PairP[P, Q, *, *], x] =
    new PairPSemigroupalCatInstanceC
}

trait PairPInstances4 {
  implicit def catPairP[P[_, _], Q[_, _]](implicit P: Cat[P], Q: Cat[Q]): Cat[PairP[P, Q, *, *]] =
    new PairPCatInstanceC
}
