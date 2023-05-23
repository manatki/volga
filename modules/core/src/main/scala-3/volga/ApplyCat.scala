package volga

trait ApplyCat[H[_, _], U[_]](using cat: SymmetricCat[H, U]) extends Aliases[H, U]:
    import cat.{unitOb, tensorOb}
    import ApplyCat.Prod
    def fromFunction[Is <: Tuple, O](f: Is => O)(using Ob[Prod[U, Is]]): H[Prod[U, Is], O]

    def pure[A](a: A): H[I, A]                                               = fromFunction[EmptyTuple, A](_ => a)
    def lift[A: Ob, B](f: A => B): H[A, B]                                   = fromFunction[Tuple1[A], B](t => f(t._1))
    def zipWith[A: Ob, B: Ob, C](f: (A, B) => C): H[A x B, C]                = fromFunction[(A, B), C](f.tupled)
    def zipWith3[A: Ob, B: Ob, C: Ob, D](f: (A, B, C) => D): H[A x B x C, D] = fromFunction[(A, B, C), D](f.tupled)

    def zipWith4[A: Ob, B: Ob, C: Ob, D: Ob, E](f: (A, B, C, D) => E): H[A x B x C x D, E] =
        fromFunction[(A, B, C, D), E](f.tupled)
end ApplyCat

object ApplyCat:
    type Prod[U[_], T <: Tuple] = T match
        case EmptyTuple      => U[tags.One]
        case a *: EmptyTuple => a
        case h1 *: h2 *: t   => Prod[U, U[tags.Tensor[h1, h2]] *: t]
