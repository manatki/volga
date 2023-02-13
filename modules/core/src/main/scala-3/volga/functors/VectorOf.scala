package volga.functors
import scala.language.implicitConversions


object VectorOf:
    opaque type VectorOf[+F[+_], +A] <: Vector[F[A]] = Vector[F[A]]

    def apply[F[+_], A](fa: Vector[F[A]]): VectorOf[F, A] = fa

    given [F[+_]: Functor]: Functor[VectorOf[F, _]] = summon[Functor[Vector]].composeFunctor[F]

    given [F[+_]: Traverse]: Traverse[VectorOf[F, _]] = summon[Traverse[Vector]].composeTraverse[F]

    given [F[+_], A]: Conversion[Vector[F[A]], VectorOf[F, A]] = x => x

end VectorOf

type VectorOf[+F[+_], +A] = VectorOf.VectorOf[F, A]