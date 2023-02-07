package volga.functors
package instances

trait TraverseMonoidal[F[+_]] extends Traverse.Cov[F], Monoidal[F]
