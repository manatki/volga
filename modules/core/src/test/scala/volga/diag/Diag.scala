package volga
package diag

import volga.tags.Obj

import volga.tags.Tensor
import volga.tags.One

enum Nat[n]:
  case Zero()             extends Nat[Zero]
  case Succ[n](p: Nat[n]) extends Nat[Succ[n]]

object Nat:
  def apply[n](using n: Nat[n]): Nat[n] = n

  def plus[n: Nat, m: Nat]: Nat[Plus[n, m]] = Nat[m] match
    case Zero()      => Nat[n]
    case m: Succ[m1] =>
      given Nat[m1] = m.p
      teq[Succ[m1], m].liftCo[Plus[n, _]].substituteCo(Succ(Nat[Plus[n, m1]](using plus)))

  type Plus[x, y] = y match
    case Zero     => x
    case Succ[y1] => Succ[Plus[x, y1]]

  infix type +[A, B] = Plus[A, B]

  def rzero[n]: Plus[n, Zero] =:= n = <:<.refl

  private def teq[A, B](using eq: A =:= B): A =:= B = eq

  inline def erase[A, B](inline proof: => A =:= B): A =:= B = <:<.refl.asInstanceOf[A =:= B]

  def lzero[n: Nat]: Plus[Zero, n] =:= n =
    Nat[n] match
      case Zero()      => <:<.refl
      case n: Succ[n1] =>
        given Nat[n1] = n.p
        teq[n, Succ[n1]].liftCo[[x] =>> Plus[Zero, x]] andThen lzero[n1].liftCo[Succ]

  def assoc[a: Nat, b: Nat, c: Nat]: Plus[Plus[a, b], c] =:= Plus[a, Plus[b, c]] =
    Nat[c] match
      case Zero()       => <:<.refl
      case c1: Succ[c1] =>
        given Nat[c1] = c1.p
        teq[c, Succ[c1]].liftCo[Plus[Plus[a, b], _]] andThen
          assoc[a, b, c1].liftCo[Succ] andThen
          teq[Succ[c1], c].liftCo[[x] =>> Plus[a, Plus[b, x]]]

  private def succPlus[a: Nat, b: Nat]: Succ[Plus[a, b]] =:= Plus[Succ[a], b] =
    Nat[b] match
      case Zero()      => <:<.refl
      case b: Succ[b1] =>
        given Nat[b1] = b.p
        teq[b, Succ[b1]].liftCo[[x] =>> Succ[Plus[a, x]]] andThen
          succPlus[a, b1].liftCo[Succ] andThen
          teq[Succ[b1], b].liftCo[[x] =>> Plus[Succ[a], x]]

  def comm[a: Nat, b: Nat]: Plus[a, b] =:= Plus[b, a] =
    Nat[b] match
      case Zero()      => lzero[a].flip
      case b: Succ[b1] =>
        given Nat[b1] = b.p
        teq[b, Succ[b1]].liftCo[[x] =>> Plus[a, x]] andThen
          comm[a, b1].liftCo[Succ] andThen
          succPlus[b1, a] andThen
          teq[Succ[b1], b].liftCo[[x] =>> Plus[x, a]]

type PropOb[T] = T match
  case tags.Obj[n]       => Nat[n]
  case tags.One          => Nat.Zero
  case tags.Tensor[a, b] => Nat.Plus[a, b]

enum FreeProp[+H[_, _], A, B]:
  case Id[A]() extends FreeProp[Nothing, A, A]
  case Embed(h: H[A, B])
  case AndThen[H[_, _], A, B, C](f: FreeProp[H, A, B], g: FreeProp[H, B, C])(using val natB: Nat[B])
      extends FreeProp[H, A, C]
  case Par[H[_, _], A, B, C, D](f: FreeProp[H, A, B], g: FreeProp[H, C, D])
      extends FreeProp[H, Nat.Plus[A, C], Nat.Plus[B, D]]

object FreeProp:
  inline def id[A, B](inline proof: => A =:= B): FreeProp[Nothing, A, B] =
    Nat.erase(proof).substituteCo(Id())

  given propCat[H[_, _]]: SymmetricCat[FreeProp[H, _, _], PropOb] with
    import Nat.{Plus, +}

    inline def idIso[A, B](inline proof: => A =:= B): A <--> B =
      Iso(FreeProp.id[A, B](proof), FreeProp.id(proof.flip))

    override given unitOb: Ob[PropOb[One]] = Nat.Zero()

    override given tensorOb[A: Nat, B: Nat]: Nat[Plus[A, B]] = Nat.plus[A, B]

    override def leftUnit[A](using PropOb[Obj[A]]): Plus[Nat.Zero, A] <--> A = idIso(Nat.lzero)

    override def braiding[A: Nat, B: Nat]: (A x B) --> (B x A) = id(Nat.comm)

    override def identity[A: Nat]: A --> A = id(<:<.refl)

    override def tensor[A: Nat, B: Nat, C: Nat, D: Nat](f: A --> B, g: C --> D): (A x C) --> (B x D) =
      FreeProp.Par(f, g)

    override def compose[A: Nat, B: Nat, C: Nat](f: B --> C, g: A --> B): A --> C = FreeProp.AndThen(g, f)

    override def assocLeft[A: Nat, B: Nat, C: Nat]: Plus[A, Plus[B, C]] --> Plus[Plus[A, B], C] =
      FreeProp.id(Nat.assoc[A, B, C].flip)

  end propCat
