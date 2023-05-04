package volga.syntax.parsing

import scala.quoted.{Quotes, Expr}
// import scala.language.`3.2`

trait VError:
    def report(): Unit

    def ++(err: VError): VError = VError.Concat(Vector(this, err))

object VError:
    private def report(using q: Quotes) = q.reflect.report

    def of(using Quotes)(message: String): VError = () => report.error(message)

    def atExpr(using Quotes)(expr: Expr[Any])(s: String): VError = () => report.error(s, expr)

    def atPos(using q: Quotes)(pos: q.reflect.Position)(s: String): VError = () => report.error(s, pos)

    def atTree(using q: Quotes)(tree: q.reflect.Tree)(s: String): VError = atPos(tree.pos)(s)

    class Concat(val errs: Vector[VError]) extends VError:
        def report(): Unit = errs.foreach(_.report())

        override def ++(err: VError): VError =
            err match
                case c: Concat => Concat(errs ++ c.errs)
                case _         => Concat(errs :+ err)

    def applyOr[A <: Matchable, B](x: A)(f: PartialFunction[A, B])(err: => VError): Either[VError, B] =
        x match
            case f(b) => Right(b)
            case _    => Left(err)

    def traverse[A <: Matchable, B](xs: Iterable[A], f: PartialFunction[A, B])(
        mkErr: A => VError
    ): Either[VError, Vector[B]] =
        var err: VError | Null = null
        val elems              = Vector.newBuilder[B]
        xs.foreach {
            case f(elem) => if err == null then elems += elem
            case x       => 
                val e = err
                err = if e != null then e ++ mkErr(x) else mkErr(x)
        }
        err match
            case null      => Right(elems.result())
            case e: VError => Left(e)

    end traverse

end VError
