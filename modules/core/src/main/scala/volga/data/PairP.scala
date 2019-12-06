package volga
package data

final case class PairP[P[_, _], Q[_, _], A, B](pab: P[A, B], qab: Q[A, B])

object PairP {}

private[volga] trait PairPCatInstance[P[_, _], Q[_, _]] extends Cat[PairP[P, Q, *, *]] {
  protected[this] val P: Cat[P]
  protected[this] val Q: Cat[Q]

  final override def id[A]: PairP[P, Q, A, A] = PairP(P.id, Q.id)

  final override def compose[A, B, C](f: PairP[P, Q, B, C], g: PairP[P, Q, A, B]): PairP[P, Q, A, C] =
    PairP(P.compose(f.pab, g.pab), Q.compose(f.qab, g.qab))
}

private[volga] class PairPCatInstanceC[P[_, _], Q[_, _]](
    implicit protected[this] val P: Cat[P],
    protected[this] val Q: Cat[Q]
) extends PairPCatInstance[P, Q]

private[volga] trait PairPArrInstance[P[_, _], Q[_, _]] extends Arr[PairP[P, Q, *, *]] with PairPCatInstance[P, Q] {
  override protected[this] val P: Arr[P]
  override protected[this] val Q: Arr[Q]

  final override def lift[A, B](f: A => B): PairP[P, Q, A, B] =
    PairP(P.lift(f), Q.lift(f))
  final override def split[A, B, C, D](f: PairP[P, Q, A, C], g: PairP[P, Q, B, D]): PairP[P, Q, (A, B), (C, D)] =
    PairP(P.split(f.pab, g.pab), Q.split(f.qab, g.qab))
}

private[volga] class PairPArrInstanceC[P[_, _], Q[_, _]](
    implicit protected[this] val P: Arr[P],
    protected[this] val Q: Arr[Q]
) extends PairPArrInstance[P, Q]

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

private[volga] class PairPArrChoiceInstanceC[P[_, _], Q[_, _]](
    implicit protected[this] val P: ArrChoice[P],
    protected[this] val Q: ArrChoice[Q]
) extends PairPArrChoiceInstance[P, Q]

private[volga] trait PairPSemigroupalCatInstance[P[_, _], Q[_, _], x[_, _]]
    extends SemigropalCat[PairP[P, Q, *, *], x] with PairPCatInstance[P, Q] {
  override protected[this] val P: SemigropalCat[P, x]
  override protected[this] val Q: SemigropalCat[Q, x]
  def assocl[A, B, C] = PairP(P.assocl, Q.assocl)

  def assocr[A, B, C] = PairP(P.assocr, Q.assocr)

  def tensor[A, B, C, D](f: PairP[P, Q, A, B], g: PairP[P, Q, C, D]): PairP[P, Q, x[A, C], x[B, D]] =
    PairP(P.tensor(f.pab, g.pab), Q.tensor(f.qab, g.qab))
}


private[volga] trait PairPMonoidalCatInstance[P[_, _], Q[_, _], x[_, _], I]
  extends MonoidalCat[PairP[P, Q, *, *], x, I] with PairPSemigroupalCatInstance[P, Q, x] {
  override protected[this] val P: MonoidalCat[P, x, I]
  override protected[this] val Q: MonoidalCat[Q, x, I]
  def lunit[A]: PairP[P, Q, x[I, A], A] = ???
  def unitl[A]: PairP[P, Q, A, x[I, A]] = ???
  def runit[A]: PairP[P, Q, x[A, I], A] = ???
  def unitr[A]: PairP[P, Q, A, x[A, I]] = ???
}

