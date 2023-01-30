package volga
package syntax

import volga.SymmetricCat

import volga.{Aliases, SymmetricCat}
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type
import volga.syntax.internal.{STerm, Pos, MParsing}
import scala.quoted.ToExpr.SetToExpr
import scala.{PartialFunction as =\>}

object smc:
    abstract final class Var[T]

    type Reconstruct[U[_], X] = X match
        case Var[a]     => a
        case EmptyTuple => U[tags.One]
        case h *: t     => U[tags.Tensor[Reconstruct[U, h], Reconstruct[U, t]]]
        case Unit       => U[tags.One]

    final abstract class SyApp[H[_, _], U[_]] extends Aliases[H, U]:
        extension [A, B](f: H[A, B]) def apply(v: Var[A]): Var[B]
        extension [B](f: H[I, B]) def apply(): Var[B]

    final class Syntax[H[_, _], U[_]](using c: SymmetricCat[H, U]) extends Aliases[H, U]:

        def dummy[A, B]: H[A, B] = null.asInstanceOf[H[A, B]]

        inline def just[R](inline block: SyApp[H, U] ?=> R): H[I, Reconstruct[U, R]] =
            ${ justMacro[H, U, R]('this)('block) }

        inline def apply[A, B](f: Var[A] => SyApp[H, U] ?=> B): H[A, Reconstruct[U, B]] =
            null.asInstanceOf[Nothing]

    end Syntax

    private def justMacro[H[_, _]: Type, U[_]: Type, R: Type](syntax: Expr[Syntax[H, U]])(
        block: Expr[SyApp[H, U] ?=> R]
    )(using Quotes): Expr[H[U[tags.One], Reconstruct[U, R]]] = SMCMacro(syntax).just(block)

    class SMCMacro[H[_, _], U[_]](syn: Expr[Syntax[H, U]])(using val q: Quotes)(using Type[H], Type[U])
        extends Aliases[H, U]:

        val p = MParsing(q)
        import p.*
        import q.reflect.*

        def just[R: Type](expr: Expr[SyApp[H, U] ?=> R]): Expr[H[I, Reconstruct[U, R]]] =
            val t  = expr.asTerm
            val tt = Expr(t.tpe.show)
            val s  = t match
                case CFBlock(Block(mids, res)) =>
                    val parsedMids = mids.collect(asMidSTerm)

                    s"""|success 
                        |${t.show(using Printer.TreeStructure)}
                        |$parsedMids""".stripMargin

                case _ =>
                    s"""|failure
                        |${expr.asTerm}""".stripMargin

            val printed = Expr(s)
            '{
                println($printed)
                println($tt)
                $syn.dummy
            }
        end just

    end SMCMacro

    def syntax[H[_, _], U[_]](using SymmetricCat[H, U]): Syntax[H, U] = Syntax()
end smc
