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

trait TEq[A1 <: AnyKind, A2 <: AnyKind]

object TEq:
    private val reflAny: TEq[Any, Any]     = new:
        def leibniz[F[_]](fa: F[Any]): F[Any] = fa
    given refl[A1 <: AnyKind]: TEq[A1, A1] = reflAny.asInstanceOf[TEq[A1, A1]]

final class Syntax[H[_, _], U[_], Plus[_, _]](using TEq[Plus, [a, b] =>> U[tags.Tensor[a, b]]]) extends Aliases[H, U]:

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
    )(using smc: SymmetricCat[H, U]): H[A x B x C x D x E, Reconstruct[U, R]] =
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
        ${ Syntax.smcMacro[H, U, Plus, I, R]('this, 'smc)('block) }

end Syntax
object Syntax:
    def smcMacro[H[_, _]: Type, U[_]: Type, Plus[_, _]: Type, A: Type, R: Type](
        syntax: Expr[Syntax[H, U, Plus]],
        smc: Expr[SymmetricCat[H, U]]
    )(
        body: Expr[SyApp[H, U] ?=> Any]
    )(using Quotes): Expr[H[A, R]] = SMCMacro[H, U, Plus](syntax, smc).smcSyntax(body)





def syntax[H[_, _], U[_], Plus[_, _]](using
    SymmetricCat[H, U],
    TEq[Plus, [a, b] =>> U[tags.Tensor[a, b]]]
): Syntax[H, U, Plus] = Syntax()
