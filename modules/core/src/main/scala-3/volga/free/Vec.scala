package volga.free

import volga.free.Nat
enum Vec[n, +A]:
    case Nil                               extends Vec[Nat.Zero, Nothing]
    case Cons[n, A](a: A, tail: Vec[n, A]) extends Vec[Nat.Succ[n], A]
