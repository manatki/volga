package volga
package syntax

object all {
  implicit class CatOps[->[_, _], A, B](val arr: A -> B) extends AnyVal{
    def andThen[C](arr2: B -> C)(implicit cat: Cat[->]): A -> C = cat.andThen(arr)(arr2)
    def >>>[C](arr2: B -> C)(implicit cat: Cat[->]): A -> C = cat.andThen(arr)(arr2)
    def split[C, D, x[_, _]](arr2: C -> D)(implicit mc: SemCatLike[->, x]): (A x C) -> (B x D) = mc.split(arr, arr2)
    def &&&[C](arr2: A -> C)(implicit inst: Arr[->]): A -> (B, C) = inst.product(arr, arr2)
  }
}