package volga.solve

sealed trait Bin[+A]

object Bin {
  final case class Branch[+A](l: Bin[A], r: Bin[A]) extends Bin[A]
  final case class Lead[+A](a: A)                   extends Bin[A]
}

sealed trait BinOp

object BinOp {
  final case object RotateL extends BinOp
  final case object RotateR extends BinOp
  final case object Swap    extends BinOp

  final case class Split(left: List[BinOp], right: List[BinOp]) extends BinOp
}
