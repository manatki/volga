package volga
package syntax.smc

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

class SMCMacro[H[_, _], U[_], Plus[_, _]](
    sym: Expr[Syntax[H, U, Plus]],
    smc: Expr[SymmetricCat[H, U]]
)(using H: Type[H], U: Type[U], P: Type[Plus], val q: Quotes)
    extends Aliases[H, U]:
    import q.reflect.*

    given MonadicTyping[q.type] with
        def one                                = TypeRepr.of[U[tags.One]]
        def tensor                             = TypeRepr.of[Plus]
        override def tensorT[a: Type, b: Type] = TypeRepr.of[U[tags.Tensor[a, b]]].dealias

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

        val adaptedRepr = adapted.mkString("\n")

        val generated = gen.generate(adapted).asExprOf[H[I, R]]

        // val s =
        //     s"""|success 
        //         |${t.show(using Printer.TreeStructure)}
        //         |------
        //         |$termsRepr
        //         |------
        //         |$stageRepr
        //         |------
        //         |$adaptedRepr""".stripMargin

        // report.info(s, expr)

        generated
    end smcSyntax
end SMCMacro