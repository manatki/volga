package volga.solve

final case class Proc[V, C[_], N](impl: Impl[V, C, N], inputs: C[V], outputs: C[V])

sealed trait Impl[V, C[_], N]

final case class Block[V, C, N](procs: Array[Proc[V, C, N]]) extends Impl[V, C, N]
final case class Call[V, C, N](name: N)                      extends Impl[V, C, N]
