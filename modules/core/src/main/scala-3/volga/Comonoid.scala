package volga

import volga.tags.Obj

trait Comonoid[A, H[_, _], U[_]] extends Aliases[H, U]:
    def drop: A --> I
    def split: A --> (A x A)

object Comonoid:
    given forCC[A, H[_, _], U[_]](using
        C: CartesianCat[H, U],
        mobj: MonoidalObjects[U],
        aob: U[Obj[A]]
    ): Comonoid[A, H, U] with
        import mobj.given
        def drop: A --> I        = C.rspawn[A] >>> C.projectRight
        def split: A --> (A x A) = C.product(C.ident, C.ident)
end Comonoid
