package volga.solve

case class Couple[+A](x: A, y: A)

// object Couple {
//   implicit def everyCouple[A, B]: PTagApply[PItems, Couple[A], Couple[B], A, B, every.type, Unit] =
//     _ => PItems.fromTraverse

//   implicit val traversable: Traverse[Couple] = new Traverse[Couple] {
//     def traverse[G[_]: Applicative, A, B](fa: Couple[A])(f: A => G[B]): G[Couple[B]] =
//       f(fa.x).map2(f(fa.y))(Couple(_, _))
//     def foldLeft[A, B](fa: Couple[A], b: B)(f: (B, A) => B): B = f(f(b, fa.x), fa.y)
//     def foldRight[A, B](fa: Couple[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
//       Eval.defer(f(fa.x, Eval.defer(f(fa.y, lb))))
//   }
// }
