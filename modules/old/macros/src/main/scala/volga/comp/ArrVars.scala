package volga.comp
import volga.syntax.comp.{varr, V}

final case class ArrVars[A, VA] private (va: VA)

object ArrVars extends LowLevelArrVars {
  implicit val varIn0: ArrVars[Unit, V[Unit]] = ArrVars(varr)

  implicit def varIn2[A, B]: ArrVars[(A, B), (V[A], V[B])]                         = ArrVars((varr, varr))
  implicit def varIn3[A, B, C]: ArrVars[(A, B, C), (V[A], V[B], V[C])]             = ArrVars((varr, varr, varr))
  implicit def varIn4[A, B, C, D]: ArrVars[(A, B, C, D), (V[A], V[B], V[C], V[D])] = ArrVars((varr, varr, varr, varr))
  implicit def varIn5[A, B, C, D, E]: ArrVars[(A, B, C, D, E), (V[A], V[B], V[C], V[D], V[E])] =
    ArrVars((varr, varr, varr, varr, varr))
}

trait LowLevelArrVars {
  implicit def varIn1[A]: ArrVars[A, V[A]] = ArrVars(varr)
}
