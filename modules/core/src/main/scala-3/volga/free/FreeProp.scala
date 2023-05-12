package volga
package free

import volga.tags.Obj

import volga.tags.Tensor
import volga.tags.One

import volga.free.Nat
type PropOb[T] = T match
    case tags.Obj[n]       => Nat[n]
    case tags.One          => Nat.Zero
    case tags.Tensor[a, b] => Nat.Plus[a, b]

enum FreeProp[+H[_, _], A, B]:
    case Id[A]()                                                                             extends FreeProp[Nothing, A, A]
    case Embed(h: H[A, B])
    case AndThen[H[_, _], A, B, C](f: FreeProp[H, A, B], g: FreeProp[H, B, C], natB: Nat[B]) extends FreeProp[H, A, C]
    case Par[H[_, _], A, B, C, D](
        f: FreeProp[H, A, B],
        g: FreeProp[H, C, D],
        A: Nat[A],
        B: Nat[B],
        C: Nat[C],
        D: Nat[D]
    )                                                                                        extends FreeProp[H, Nat.Plus[A, C], Nat.Plus[B, D]]
    case Swap[A, B](natA: Nat[A], natB: Nat[B])                                              extends FreeProp[Nothing, Nat.Plus[A, B], Nat.Plus[B, A]]

    def link0[H_, H1[x, y] >: H[x, y] <: H_](using A0: A =:= Nat.Zero, B0: B =:= Nat.Zero): Vector[(H_, H_)] =
        val h0 = A0.liftCo[FreeProp[H, _, B]].andThen(B0.liftCo[FreeProp[H, Nat.Zero, _]])(this)
        FreeProp.link[Nat.Zero, Nat.Zero, H_, H1](h0, EmptyTuple)._2

    def link[H_, H1[x, y] >: H[x, y] <: H_](using Nat[A], Nat[B])(
        ins: Nat.Vec[A, H_]
    ): (Nat.Vec[B, H_], Vector[(H_, H_)]) =
        FreeProp.link[A, B, H_, H1](this, ins)

end FreeProp

object FreeProp:
    def idInt[x <: Int]: FreeProp[Nothing, Nat.OfInt[x], Nat.OfInt[x]] = id

    inline def id[A, B](using inline proof: => A =:= B): FreeProp[Nothing, A, B] =
        Nat.erase(proof).substituteCo(Id())

    val swap2: FreeProp[Nothing, Nat.`2`, Nat.`2`] =
        val one = Nat.Succ(Nat.Zero())
        Swap(one, one)

    given propCat[H[_, _]]: SymmetricCat[FreeProp[H, _, _], PropOb] with
        import Nat.{Plus, +}

        inline def idIso[A, B](using inline proof: => A =:= B): A <--> B =
            Iso(FreeProp.id[A, B], FreeProp.id(using proof.flip))

        override given unitOb: Ob[PropOb[One]] = Nat.Zero()

        override given tensorOb[A: Nat, B: Nat]: Nat[Plus[A, B]] = Nat.plus[A, B]

        override def leftUnit[A](using PropOb[Obj[A]]): Plus[Nat.Zero, A] <--> A = idIso(using Nat.lzero)

        override def braiding[A: Nat, B: Nat]: (A x B) --> (B x A) = Swap(summon, summon)

        override def identity[A: Nat]: A --> A = id

        override def tensor[A: Nat, B: Nat, C: Nat, D: Nat](f: A --> B, g: C --> D): (A x C) --> (B x D) =
            FreeProp.Par(f, g, summon, summon, summon, summon)

        override def compose[A: Nat, B: Nat, C: Nat](f: B --> C, g: A --> B): A --> C = FreeProp.AndThen(g, f, summon)

        override def assocLeft[A: Nat, B: Nat, C: Nat]: Plus[A, Plus[B, C]] --> Plus[Plus[A, B], C] =
            FreeProp.id(using Nat.assoc[A, B, C].flip)

    end propCat

    def link[N: Nat, M: Nat, H_, H[_, _] <: H_](
        p: FreeProp[H, N, M],
        inbound: Nat.Vec[N, H_]
    ): (Nat.Vec[M, H_], Vector[(H_, H_)]) =
        p match
            case Id()                                                              => (inbound, Vector.empty)
            case Swap(given Nat[a], given Nat[b])                                  =>
                val (va, vb) = Nat.Vec.split[a, b, H_](inbound)
                (vb.concat(va), Vector.empty)
            case Embed(h)                                                          =>
                val links    = inbound.toVector.map((_, h))
                val outbound = Nat.Vec.replicate[M, H_](h)
                (outbound, links)
            case AndThen(f, g, given Nat[k])                                       =>
                val (mid, links1) = link[N, k, H_, H](f, inbound)
                val (out, links2) = link[k, M, H_, H](g, mid)
                (out, links1 ++ links2)
            case Par(f, g, given Nat[a], given Nat[b], given Nat[c], given Nat[d]) =>
                val (in1, in2)     = Nat.Vec.split[a, c, H_](inbound)
                val (out1, links1) = link[a, b, H_, H](f, in1)
                val (out2, links2) = link[c, d, H_, H](g, in2)
                (out1.concat(out2), links1 ++ links2)

end FreeProp
