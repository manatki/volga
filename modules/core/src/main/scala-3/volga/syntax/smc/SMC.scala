package volga
package syntax
package smc

import volga.SymmetricCat

import volga.{Aliases, SymmetricCat}
import scala.quoted.Quotes
import scala.quoted.Expr
import scala.quoted.Type
import volga.syntax.parsing.{Pos, MParsing, MGeneration, MonadicTyping}
import volga.syntax.parsing.STerm
import scala.quoted.ToExpr.SetToExpr
import scala.{PartialFunction as =\>}
import scala.collection.View.Empty
import volga.free.Nat
import volga.syntax.parsing.VError
import scala.util.chaining.given
import volga.syntax.solve.StageList
import scala.compiletime.summonInline

abstract final class V[T]

type Reconstruct[U[_], X] = X match
    case V[a]       => a
    case EmptyTuple => U[tags.One]
    case h *: t     => U[tags.Tensor[Reconstruct[U, h], Reconstruct[U, t]]]
    case Unit       => U[tags.One]
    case EmptyTuple => U[tags.One]

type TResults[U[_], X] <: Tuple = X match
    case Nat.Zero             => EmptyTuple
    case Nat.Succ[n]          => V[Nat.`1`] *: TResults[U, n]
    case U[tags.Tensor[u, v]] => Results[U, u] *: TResults[U, v]
    case U[tags.One]          => EmptyTuple
    case _                    => V[X] *: EmptyTuple

type Results[U[_], X] = X match
    case Nat.Zero             => Unit
    case Nat.Succ[n]          => V[Nat.`1`] *: TResults[U, n]
    case U[tags.Tensor[u, v]] => Results[U, u] *: TResults[U, v]
    case U[tags.One]          => Unit
    case _                    => V[X]

final abstract class SyApp[H[_, _], U[_]] extends Aliases[H, U]:
    extension [A, B](f: H[A, B]) def apply(v: V[A]): Results[U, B]
    extension [B](f: H[I, B]) def apply(): V[B]

final class Syntax[H[_, _], U[_]] extends Aliases[H, U]:

    def dummy[A, B]: H[A, B] = null.asInstanceOf[H[A, B]]

    inline def of0[R](inline block: SyApp[H, U] ?=> R)(using SymmetricCat[H, U]): H[I, Reconstruct[U, R]] =
        syntax(block)

    inline def of1[A, B](inline f: SyApp[H, U] ?=> V[A] => B)(using SymmetricCat[H, U]): H[A, Reconstruct[U, B]] =
        syntax(f)

    inline def of2[A, B, R](inline f: SyApp[H, U] ?=> (V[A], V[B]) => R)(using
        SymmetricCat[H, U]
    ): H[A x B, Reconstruct[U, R]] =
        syntax(f)

    inline def of3[A, B, C, R](inline f: SyApp[H, U] ?=> (V[A], V[B], V[C]) => R)(using
        SymmetricCat[H, U]
    ): H[A x B x C, Reconstruct[U, R]] =
        syntax(f)

    inline def of4[A, B, C, D, R](
        inline f: SyApp[H, U] ?=> (V[A], V[B], V[C], V[D]) => R
    )(using SymmetricCat[H, U]): H[A x B x C x D, Reconstruct[U, R]] =
        syntax(f)

    inline def of5[A, B, C, D, E, R](
        inline f: SyApp[H, U] ?=> (V[A], V[B], V[C], V[D], V[E]) => R
    )(using SymmetricCat[H, U]): H[A x B x C x D x E, Reconstruct[U, R]] =
        syntax(f)

    inline def of6[A, B, C, D, E, F, R](
        inline f: SyApp[H, U] ?=> (V[A], V[B], V[C], V[D], V[E], V[F]) => R
    )(using SymmetricCat[H, U]): H[A x B x C x D x E x F, Reconstruct[U, R]] =
        syntax(f)

    inline def of7[A, B, C, D, E, F, G, R](
        inline f: SyApp[H, U] ?=> (V[A], V[B], V[C], V[D], V[E], V[F], V[G]) => R
    )(using SymmetricCat[H, U]): H[A x B x C x D x E x F x G, Reconstruct[U, R]] =
        syntax(f)

    private inline def syntax[I, R](inline block: SyApp[H, U] ?=> Any)(using smc: SymmetricCat[H, U]): H[I, R] =
        ${ Syntax.smcMacro[H, U, I, R]('this, 'smc)('block) }

end Syntax
object Syntax:
    def smcMacro[H[_, _]: Type, U[_]: Type, A: Type, R: Type](
        syntax: Expr[Syntax[H, U]],
        smc: Expr[SymmetricCat[H, U]]
    )(
        body: Expr[SyApp[H, U] ?=> Any]
    )(using Quotes): Expr[H[A, R]] = SMCMacro[H, U](syntax, smc).smcSyntax(body)

class SMCMacro[H[_, _], U[_]](
    sym: Expr[Syntax[H, U]],
    smc: Expr[SymmetricCat[H, U]]
)(using Type[H], Type[U])(using val q: Quotes)
    extends Aliases[H, U]:
    import q.reflect.*

    given MonadicTyping[q.type] with
        def one  = TypeRepr.of[U[tags.One]]
        def tensor = TypeRepr.of[[a, b] =>> U[tags.Tensor[a, b]]]

    val parse = MParsing()
    val gen   = MGeneration(smc)

    def smcSyntax[I: Type, R: Type](expr: Expr[Any]): Expr[H[I, R]] =
        val t = expr.asTerm

        val (vars, mterms, fterm) = parse.parseBlock(t) match
            case Left(verror)  => verror.reportAndAbort()
            case Right(values) => values

        val stageList = StageList.fromTerms(vars, mterms, fterm)

        val adapted = StageList.withAdaptation(stageList) match
            case Left(e)     => e.reportAndAbort
            case Right(vals) => vals

        val termsRepr = (mterms :+ fterm).view.mkString("\n")

        val stageRepr = stageList.mkString("\n")

        // val xxx = stageList.view.flatMap{x => 
        //    x.next
        // }.collect{ 
        //     case x if x.typ.isEmpty => x.name
        // }.mkString("\n")

        // if xxx.nonEmpty then
        //     report.warning(s"untyped vars:\n$xxx")

        val adaptedRepr = adapted.mkString("\n")

        val generated = gen.generate(adapted)

        val s =
            s"""|success 
                |${t.show(using Printer.TreeStructure)}
                |------
                |$termsRepr
                |------
                |$stageRepr
                |------
                |$adaptedRepr""".stripMargin

        report.info(s, expr)

        '{ $sym.dummy[I, R] }
    end smcSyntax

end SMCMacro

def syntax[H[_, _], U[_]](using SymmetricCat[H, U]): Syntax[H, U] = Syntax()
