package volga.functors
package instances

trait TraverseMonad[F[+_]] extends Traverse[F], Monad[F]
