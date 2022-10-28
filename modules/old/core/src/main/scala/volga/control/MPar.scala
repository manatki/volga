package volga.control

import cats.{Monad, Parallel}

object MPar extends LowPriorMPar {
  type TC[F[_]] <: Parallel[F]

  implicit def instanceByParalel[F[_]](implicit F: Parallel[F]): TC[F] = F.asInstanceOf[TC[F]]
}

trait LowPriorMPar { self: MPar.type =>
  implicit def instanceByMonad[F[_]: Monad]: TC[F] = Parallel.identity[F].asInstanceOf[TC[F]]
}
