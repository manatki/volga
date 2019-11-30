//package volga.free
//
//sealed trait FreeArr[+R[p[_, _, _], a, b], +P[_, _, _], A, B]
//sealed trait FreeSMC[+R[p[_, _, _], a, b], +P[_, _, _], A, B]
//
//sealed trait FreeTensorCat[+R[p[_, _, _], a, b], +P[_, _, _], A, B] extends FreeArr[R, P, A, B] with FreeSMC[R, P, A, B]
//
//sealed trait FreeCat[+R[p[_, _, _], a, b], +P[_, _, _], A, B] extends FreeTensorCat[R, P, A, B]
//
//object Free {
//
//  final abstract class Morhp
//  final abstract class Tensor
//
//  final case class Cat[->[_, _], A, B](free: FreeCat[Cat, ->, A, B]) extends AnyVal
//
//  final case class Embed[P[_, _], A, B](p: A P B) extends FreeCat[Nothing, P, A, B]
//
//  final case class Id[A]() extends FreeCat[Nothing, Nothing, A, A]
//
//  final case class Compose[+R[p[_, _], a, b], P[_, _], A, B, C](
//      begin: R[P, A, B],
//      end: R[P, A, B],
//  ) extends FreeCat[R, P, A, B]
//
//  final case class Lift[A, B](f: A => B) extends FreeArr[Nothing, Nothing, A, B]
//  final case class Split[+R[p[_, _], t[_, _], a, b], +P[_, _], +T[_, _], A, B, C, D](
//      first: R[P, T, A, B],
//      second: R[P, T, C, D]
//  ) extends FreeSMC[R, P, T, A, B]
//
//}
