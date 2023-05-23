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
import tags.{One, Tensor as T}

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

    sealed trait Monoidal[U[_], Dom, Codom]
    sealed trait Symmetric[U[_], Dom, Codom]   extends Monoidal[U, Dom, Codom]
    sealed trait Cartesian[U[_], Dom, Codom]   extends Symmetric[U, Dom, Codom]
    sealed trait Cocartesian[U[_], Dom, Codom] extends Symmetric[U, Dom, Codom]

    case class SpawnLeft[U[_], A]()        extends Monoidal[U, A, U[T[U[One], A]]]
    case class DropLeft[U[_], A]()         extends Monoidal[U, U[T[U[One], A]], A]
    case class DropRight[U[_], A]()        extends Monoidal[U, U[T[A, U[One]]], A]
    case class AssocLeft[U[_], A, B, C]()  extends Monoidal[U, U[T[A, U[T[B, C]]]], U[T[U[T[A, B]], C]]]
    case class AssocRight[U[_], A, B, C]() extends Monoidal[U, U[T[U[T[A, B]], C]], U[T[A, U[T[B, C]]]]]
    case class Braiding[U[_], A, B]()      extends Symmetric[U, U[T[A, B]], U[T[B, A]]]
    case class Copy[U[_], A]()             extends Cartesian[U, A, U[T[A, A]]]
    case class Delete[U[_], A]()           extends Cartesian[U, A, U[One]]
    case class Merge[U[_], A]()            extends Cocartesian[U, U[T[A, A]], A]
    case class Generate[U[_], A]()         extends Cocartesian[U, U[One], A]
end FreeCat
