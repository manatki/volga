package volga
package syntax

trait all extends Arr.ToArrOps
  with Cat.ToCatOps
  with ArrPlus.ToArrPlusOps
  with ArrChoice.ToArrChoiceOps
  with SymonOps

object arr extends Arr.ToArrOps
object cat extends Cat.ToCatOps
object plus extends ArrPlus.ToArrPlusOps
object choice extends ArrChoice.ToArrChoiceOps
object symmon extends SymonOps
object all extends all