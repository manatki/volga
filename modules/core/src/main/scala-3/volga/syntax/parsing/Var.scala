package volga
package syntax.parsing

import scala.quoted.Quotes
import volga.syntax.solve.PMagma

trait Var[+q <: Quotes & Singleton](val q: q):
    self =>
    import q.reflect.*
    def name: String
    def typ: Option[TypeRepr]
    def mndType: MndType[q, TypeRepr] = MndType(typ)

    def replenish(tgt: Var[q.type]): Var[q.type] = new Var(q):
        override val name: String = self.name
        override val typ          = self.typ.orElse(tgt.typ)
end Var

object Var:
    given variable[q <: Quotes & Singleton](using
        q: q,
        MT: MonadicTyping[q.type]
    ): Variable[Var[q.type], MndType[q, q.reflect.TypeRepr]] = new:
        type Label = String

        override def label(v: Var[q.type]): Label = v.name

        override def describe(v: Var[q.type]): MndType[q, q.reflect.TypeRepr] = v.mndType

        override def replenish(src: Var[q.type], tgt: Var[q.type]) = src.replenish(tgt)

end Var

final class Vars[q <: Quotes & Singleton](using val q: q):
    import q.reflect.*

    private val VSym = TypeRepr.of[volga.syntax.smc.V[?]].typeSymbol
    private case class TyVar(name: String, typ: Option[TypeRepr] = None) extends Var[q.type](q):
        override def toString: String = typ match
            case None    => name
            case Some(t) => s"$name: ${simp(t).show}"

        private def simp(t: TypeRepr): TypeRepr = t match
            case AppliedType(t, ts) => AppliedType(t.dealias, ts.map(simp))
            case t                  => t.dealias
    end TyVar

    def varOf(name: String, t: TypeTree): Var[q] =
        import q.reflect.*
        t.tpe.baseType(VSym) match
            case AppliedType(t1, List(t2)) => TyVar(name, Some(t2))
    end varOf

    def varNamed(name: String): Var[q] = TyVar(name)

end Vars
