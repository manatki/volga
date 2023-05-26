package volga
package free

import volga.Aliases

package features:
    abstract class Mon
    abstract class Sym    extends Mon
    abstract class Cart   extends Sym
    abstract class Cocart extends Sym
    abstract class Apply

import features.*
import tags.{One, Obj, Tensor as T, Scala}

sealed trait FreeCat[+U[_], +Q[_, _], Dom, Codom]

object FreeCat:
    case class Parallel[U[_], +Q[_, _], A, B, C, D](
        f: FreeCat[U, Q, A, B],
        g: FreeCat[U, Q, C, D]
    ) extends FreeCat[U, Q, U[T[A, C]], U[T[B, D]]]

    case class Sequential[+U[_], +Q[_, _], A, B, C](
        f: FreeCat[U, Q, A, B],
        g: FreeCat[U, Q, B, C]
    ) extends FreeCat[U, Q, A, C]

    case class Ident[U[_], A]()                        extends FreeCat[U, Nothing, A, A]
    case class Embed[U[_], +Q[_, _], A, B](f: Q[A, B]) extends FreeCat[U, Q, A, B]

    sealed trait Monoidal[U[_], X, Y]
    sealed trait Symmetry[U[_], Dom, Codom]
    sealed trait AllComonoids[U[_], Dom, Codom]
    sealed trait AllMonoids[U[_], Dom, Codom]
    sealed trait Lifting[U[_], Dom, Codom]
    sealed trait Application[U[_], Dom, Codom]

    type Symmetric[U[_], X, Y]   = Monoidal[U, X, Y] | Symmetry[U, X, Y]
    type Cartesian[U[_], X, Y]   = Monoidal[U, X, Y] | AllComonoids[U, X, Y]
    type Cocartesian[U[_], X, Y] = Monoidal[U, X, Y] | AllMonoids[U, X, Y]
    type Scalian[U[_], X, Y]     = Symmetric[U, X, Y] | Lifting[U, X, Y] | Application[U, X, Y]

    type oo[U[_], A] = U[Obj[A]]
    type ss[U[_], A] = U[Scala[A]]
    case class SpawnLeft[U[_], A]()(using U oo A)           extends Monoidal[U, A, U[T[U[One], A]]]
    case class SpawnRight[U[_], A]()(using U oo A)          extends Monoidal[U, A, U[T[A, U[One]]]]
    case class DropLeft[U[_], A]()(using U oo A)            extends Monoidal[U, U[T[U[One], A]], A]
    case class DropRight[U[_], A]()(using U oo A)           extends Monoidal[U, U[T[A, U[One]]], A]
    case class AssocLeft[U[_], A, B, C]()(using U oo A, U oo B, U oo C)
        extends Monoidal[U, U[T[A, U[T[B, C]]]], U[T[U[T[A, B]], C]]]
    case class AssocRight[U[_], A, B, C]()(using U oo A, U oo B, U oo C)
        extends Monoidal[U, U[T[U[T[A, B]], C]], U[T[A, U[T[B, C]]]]]
    case class Braiding[U[_], A, B]()(using U oo A, U oo B) extends Symmetry[U, U[T[A, B]], U[T[B, A]]]
    case class Copy[U[_], A]()(using U oo A)                extends AllComonoids[U, A, U[T[A, A]]]
    case class Delete[U[_], A]()(using U oo A)              extends AllComonoids[U, A, U[One]]
    case class Merge[U[_], A]()(using U oo A)               extends AllMonoids[U, U[T[A, A]], A]
    case class Generate[U[_], A]()(using U oo A)            extends AllMonoids[U, U[One], A]
    case class Lift[U[_], A, B](f: A => B)                  extends Lifting[U, U ss A, U ss B]
    case class ScalaUnit[U[_]]()                            extends Application[U, U[One], U ss Unit]
    case class Zip[U[_], A, B]()                            extends Application[U, U[T[U ss A, U ss B]], U ss (A, B)]

    private class CatInstance[U[_], Q[x, y]] extends Cat[FreeCat[U, Q, _, _], U]:
        def identity[A: Ob]: A --> A                                      = Ident()
        def compose[A: Ob, B: Ob, C: Ob](f: B --> C, g: A --> B): A --> C = Sequential(g, f)
    end CatInstance

    given catInstance[Q[x, y], U[_]]: Cat[FreeCat[U, Q, _, _], U] = new CatInstance
    private class MonoidalCatInstance[U[_], Q[x, y] >: Monoidal[U, x, y]](using MonoidalObjects[U])
        extends CatInstance[U, Q]
        with MonoidalCat[FreeCat[U, Q, _, _], U]:

        override def associate[A: Ob, B: Ob, C: Ob]: (A x (B x C)) <--> (A x B x C) =
            Iso(Embed(AssocLeft()), Embed(AssocRight()))

        override def leftUnit[A: Ob]: U[T[U[One], A]] <--> A = Iso(Embed(DropLeft()), Embed(SpawnLeft()))

        override def rightUnit[A: Ob]: U[T[A, U[One]]] <--> A = Iso(Embed(DropRight()), Embed(SpawnRight()))

        override def tensor[A: Ob, B: Ob, C: Ob, D: Ob](f: A --> B, g: C --> D): (A x C) --> (B x D) = Parallel(f, g)
    end MonoidalCatInstance

    given monoidalCatInstance[Q[x, y] >: Monoidal[U, x, y], U[_]](using
        MonoidalObjects[U]
    ): MonoidalCat[FreeCat[U, Q, _, _], U] =
        new MonoidalCatInstance

    private class SymmetricCatInstance[U[_], Q[x, y] >: Symmetric[U, x, y]](using MonoidalObjects[U])
        extends MonoidalCatInstance[U, Q]
        with SymmetricCat[FreeCat[U, Q, _, _], U]:

        def braiding[A: Ob, B: Ob]: FreeCat[U, Q, U[T[A, B]], U[T[B, A]]]                           = Embed(Braiding())
        def assocLeft[A: Ob, B: Ob, C: Ob]: FreeCat[U, Q, U[T[A, U[T[B, C]]]], U[T[U[T[A, B]], C]]] = Embed(AssocLeft())
    end SymmetricCatInstance

    given symmetricCatInstance[Q[x, y] >: Symmetric[U, x, y], U[_]](using
        MonoidalObjects[U]
    ): SymmetricCat[FreeCat[U, Q, _, _], U] =
        new SymmetricCatInstance

    private trait LiftingCatInsstance[U[_], Q[x, y] >: Lifting[U, x, y]]
        extends CatInstance[U, Q]
        with ScalaFunctor[FreeCat[U, Q, _, _], U]:
        def lift[A, B](f: A => B): $[A] --> $[B] = Embed(Lift(f))

    given liftingCatInstance[Q[x, y] >: Lifting[U, x, y], U[_]](using
        ScalaObjects[U]
    ): ScalaFunctor[FreeCat[U, Q, _, _], U] = new LiftingCatInsstance {}

    private class ApplyCatInstance[U[_], Q[x, y] >: Scalian[U, x, y]](using MonoidalObjects[U], ScalaObjects[U])
        extends SymmetricCatInstance[U, Q]
        with LiftingCatInsstance[U, Q]
        with ApplyCat[FreeCat[U, Q, _, _], U]:

        def scalaUnit: I --> $[Unit] = Embed(ScalaUnit())

        def zip[A, B]: ($[A] x $[B]) --> $[(A, B)] = Embed(Zip())
    end ApplyCatInstance

    given applyCatInstance[Q[x, y] >: Scalian[U, x, y], U[_]](using
        MonoidalObjects[U],
        ScalaObjects[U]
    ): ApplyCat[FreeCat[U, Q, _, _], U] = new ApplyCatInstance

end FreeCat
