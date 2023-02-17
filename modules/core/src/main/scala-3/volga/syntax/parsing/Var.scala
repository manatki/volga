package volga.syntax.parsing

import scala.quoted.Quotes
import volga.syntax.solve.PMagma

trait Var[+q <: Quotes & Singleton](val q: q):
    import q.reflect.*
    def name: String
    def typ: Option[TypeRepr]
    def mndType: MndType[q, TypeRepr] = MndType(typ)

object Var:
    given variable[q <: Quotes & Singleton](using
        q: q,
        MT: MonadicTyping[q.type]
    ): Variable[Var[q.type], MndType[q, q.reflect.TypeRepr]] = new:
        type Label = String

        override def label(v: Var[q.type]): Label = ???

        override def describe(v: Var[q.type]): MndType[q, q.reflect.TypeRepr] = v.mndType

end Var

final class Vars[q <: Quotes & Singleton](using val q: q):
    import q.reflect.*
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
        t.tpe match
            case AppliedType(t1, List(t2)) => TyVar(name, Some(t2))
            case _                         => TyVar(name)
    end varOf

    def varNamed(name: String): Var[q] = TyVar(name)

end Vars
