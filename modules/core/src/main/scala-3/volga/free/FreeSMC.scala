package volga.free

import volga.tags.Tensor

import volga.tags.One

import volga.tags.Hom

import volga.{ObAliases, Aliases, Iso, SymmetricCat}
import volga.tags.{Hom, One, Tensor}
abstract class FreeSMC[Q[_, _], U[_]] extends ObAliases[U]:
  def unitObj: Ob[I]
  def tensorObj[A, B](using Ob[A], Ob[B]): Ob[A x B]

  enum Free[X, Y]:
    case Embed[A, B](f: Q[A, B])                      extends (A --> B)
    case SpawnLeft[A]()                               extends (A --> (I x A))
    case DropLeft[A]()                                extends ((I x A) --> A)
    case Assoc[A, B, C]()                             extends (((A x B) x C) --> (A x (B x C)))
    case Parallel[A, B, C, D](f: A --> B, g: C --> D) extends ((A x C) --> (B x D))
    case Sequential[A, B, C](f: A --> B, g: B --> C)  extends (A --> C)
    case Braiding[A, B]()                             extends ((A x B) --> (B x A))
    case Ident[A]()                                   extends (A --> A)

  object FreeHom extends Aliases[Free, U]

  def embed[A, B](h: Q[A, B]): Free[A, B] = Free.Embed(h)

  object Free extends Aliases[Free, U]:
    import FreeHom.*
    given freeSmc: SymmetricCat[Free, U] with

      override def assocLeft[A: Ob, B: Ob, C: Ob]: (A x (B x C)) --> ((A x B) x C) = ???

      override def unitOb: Ob[I]                                                                 = unitObj
      override def tensorOb[A: Ob, B: Ob]: Ob[A x B]                                             = tensorObj
      override def compose[A: Ob, B: Ob, C: Ob](f: Free[B, C], g: Free[A, B]): A --> C = Sequential(g, f)

      override def identity[A: Ob]: A --> A = Ident()

      override def tensor[A: Ob, B: Ob, C: Ob, D: Ob](f: A --> B, g: C --> D): (A x C) --> (B x D) = Parallel(f, g)

      override def leftUnit[A: Ob]: (I x A) <--> A = Iso(DropLeft(), SpawnLeft())

      override def braiding[A: Ob, B: Ob]: (A x B) --> (B x A) = Braiding()
    end freeSmc
