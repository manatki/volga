package volga.comp
import volga.syntax.comp.{varr, V}

final case class SMCVars[A, VA, x[_, _], I] private (va: VA)

object SMCVars extends LowLevelSMCVars {
  implicit def varIn0[x[_, _], I]: SMCVars[I, V[I], x, I] = SMCVars(varr)

  implicit def varIn2[x[_, _], I, A, B]: SMCVars[(A x B), (V[A], V[B]), x, I] = SMCVars((varr, varr))
  implicit def varIn3[x[_, _], I, A, B, C]: SMCVars[(A x B x C), (V[A], V[B], V[C]), x, I] =
    SMCVars((varr, varr, varr))
  implicit def varIn4[x[_, _], I, A, B, C, D]: SMCVars[(A x B x C x D), (V[A], V[B], V[C], V[D]), x, I] =
    SMCVars((varr, varr, varr, varr))
  implicit def varIn5[x[_, _], I, A, B, C, D, E]: SMCVars[(A x B x C x D x E), (V[A], V[B], V[C], V[D], V[E]), x, I] =
    SMCVars((varr, varr, varr, varr, varr))
}

trait LowLevelSMCVars {
  implicit def varIn1[x[_, _], I, A]: SMCVars[A, V[A], x, I] = SMCVars(varr)
}
