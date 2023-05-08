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

    inline def just[R](inline block: SyApp[H, U] ?=> R): H[I, Reconstruct[U, R]] =
        ${ justMacro[H, U, R]('this)('block) }

    inline def apply[A, B](inline f: SyApp[H, U] ?=> V[A] => B): H[A, Reconstruct[U, B]] =
        ${ applyMacro[H, U, A, B]('this)('f) }

end Syntax

def justMacro[H[_, _]: Type, U[_]: Type, R: Type](syntax: Expr[Syntax[H, U]])(
    block: Expr[SyApp[H, U] ?=> R]
)(using Quotes): Expr[H[U[tags.One], Reconstruct[U, R]]] = SMCMacro(syntax).block(block)

def applyMacro[H[_, _]: Type, U[_]: Type, A: Type, R: Type](syntax: Expr[Syntax[H, U]])(
    lam: Expr[SyApp[H, U] ?=> V[A] => R]
)(using Quotes): Expr[H[A, Reconstruct[U, R]]] = SMCMacro[H, U](syntax).lambda[A, R](lam)

class SMCMacro[H[_, _], U[_]](syn: Expr[Syntax[H, U]])(using val q: Quotes)(using Type[H], Type[U])
    extends Aliases[H, U]:
    import q.reflect.*

    given MonadicTyping[q.type] with
        def ident  = TypeRepr.of[U[tags.One]]
        def tensor = TypeRepr.of[[a, b] =>> U[tags.Tensor[a, b]]]

    val parse = MParsing()
    val gen   = MGeneration()

    def block[R](expr: Expr[SyApp[H, U] ?=> R])(using Type[I], Type[R]): Expr[H[I, Reconstruct[U, R]]] =
        smcSyntax(expr)

    def lambda[A, R](expr: Expr[SyApp[H, U] ?=> V[A] => R])(using Type[A], Type[R]): Expr[H[A, Reconstruct[U, R]]] =
        smcSyntax(expr)

    private def smcSyntax[I: Type, R: Type](expr: Expr[Any]): Expr[H[I, R]] =
        val t = expr.asTerm

        val (vars, mterms, fterm) = parse.parseBlock(Nil, t) match
            case Left(verror)  =>
                verror.reportAndAbort()
            case Right(values) => values

        val stageList = StageList.fromTerms(vars, mterms, fterm)

        val res = (mterms :+ fterm).view.mkString("\n")

        val s =
            s"""|success 
                |${t.show(using Printer.TreeStructure)}
                |------
                |$res""".stripMargin

        report.info(s, expr)

        '{ $syn.dummy[I, R] }
    end smcSyntax

end SMCMacro

def syntax[H[_, _], U[_]](using SymmetricCat[H, U]): Syntax[H, U] = Syntax()
