package volga.syntax.parsing

import scala.quoted.{Quotes, Expr}
// import scala.language.`3.2`

trait VError:
    def report(): Unit

    def reportAndAbort(): Nothing

    def ++(err: VError): VError = VError.Concat(Vector(this, err))

object VError:
    private def rep(using q: Quotes) = q.reflect.report

    def of(using Quotes)(message: String): VError = new:
        def report()         = rep.error(message)
        def reportAndAbort() = rep.errorAndAbort(message)

    def atTree(using q: Quotes)(s: String)(tree: q.reflect.Tree): VError = new:
        def report()         = rep.error(s"$s\n $tree", tree.pos)
        def reportAndAbort() = rep.errorAndAbort(s"$s\n $tree", tree.pos)

    class Concat(val errs: Vector[VError]) extends VError:
        def report(): Unit            = errs.foreach(_.report())
        def reportAndAbort(): Nothing =
            errs.init.foreach(_.report())
            errs.last.reportAndAbort()

        override def ++(err: VError): VError =
            err match
                case c: Concat => Concat(errs ++ c.errs)
                case _         => Concat(errs :+ err)
    end Concat

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
