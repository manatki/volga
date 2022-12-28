package volga
package prob

import volga.tags.Scala
import volga.tags.Obj
import volga.tags.Tensor
import volga.tags.One

enum MarkQ[A, B]:
  case Range(n: Int)         extends (I --> $[Int])
  case Lift[A, B](f: A => B) extends ($[A] --> $[B])
  case Zip[A, B]()           extends (($[A] x $[B]) --> $[(A, B)])
  case Unzip[A, B]()         extends ($[(A, B)] --> ($[A] x $[B]))
  case Split[A, B]()         extends ($[Either[A, B]] --> ($[A] x $[B]))
  case Merge[A, B]()         extends (($[A] x $[B]) --> $[Either[A, B]])
  case Fail                  extends ($[Unit] --> I)

object MarkQ extends Aliases[MarkQ, FreeU]:
  given HasScalaFunctor[Mark, FreeU] with
    def lift[A, B](f: A => B): $[A] --> $[B] = Mark.lift(f)

type Mark[A, B] = Mark.Free[A, B]

object Mark extends FreeSMC[MarkQ, FreeU] with Aliases[Mark, FreeU]:

  def unitObj: Ob[I] = FreeU.forOne

  def tensorObj[A: Ob, B: Ob]: Ob[A x B] = FreeU.forTensor

  def range(x: Int): I --> $[Int]                      = embed(MarkQ.Range(x))
  def lift[A, B](f: A => B): $[A] --> $[B]             = embed(MarkQ.Lift(f))
  def zip[A, B]: (($[A] x $[B]) --> $[(A, B)])         = embed(MarkQ.Zip())
  def unzip[A, B]: ($[(A, B)] --> ($[A] x $[B]))       = embed(MarkQ.Unzip())
  def split[A, B]: ($[Either[A, B]] --> ($[A] x $[B])) = embed(MarkQ.Split())
  def merge[A, B]: (($[A] x $[B]) --> $[Either[A, B]]) = embed(MarkQ.Merge())
  def fail: $[Unit] --> I                              = embed(MarkQ.Fail)

  def zipWith[A, B, C](f: (A, B) => C): ($[A] x $[B]) --> $[C]         = zip >>> lift(f.tupled)
  def mergeWith[A, B, C](f: A => C, g: B => C): ($[A] x $[B]) --> $[C] = merge >>> lift(_.fold(f, g))
  def filter[A](p: A => Boolean): $[A] --> $[A]                        =
    lift((x: A) => if p(x) then Right(x) else Left(())) >>> split >>> (fail >< ident) >>> lconsume
  val range1                                                           = range(1) // event with guaranteed prob = 1
  val unit: I --> $[Unit]                                              = range1 >>> lift(_ => ())

  def failAll[A: Ob]: A --> I = ob[A] match
    case _: FreeObj.Scala[a]   => lift((_: a) => ()) >>> fail
    case FreeObj.One           => ident
    case p: FreeObj.Prod[x, y] => 
      given Ob[x] = p.l
      given Ob[y] = p.r
      (failAll[x] >< failAll[y]) >>> rconsume