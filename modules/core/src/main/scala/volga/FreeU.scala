package volga

enum FreeU[+X]:
  case Obj extends FreeU[tags.Obj[Nothing]]

object FreeU:
  given [A]: FreeU[tags.Obj[A]] = FreeU.Obj
