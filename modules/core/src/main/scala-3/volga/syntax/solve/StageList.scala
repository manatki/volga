package volga.syntax.solve
import volga.syntax.parsing.{Pos, STerm, Variable, Labeled}

import volga.syntax.solve.Bin
import volga.syntax.solve.PMagma
import volga.syntax.solve.Adaptation
import volga.functors.Traverse
import scala.annotation.tailrec
import volga.syntax.solve.BinRes
import volga.functors.VectorOf
import java.awt.FocusTraversalPolicy
import volga.functors.Monoidal
import scala.language.implicitConversions
import volga.syntax.solve.BinOp
import volga.util.collections.*
import volga.syntax.parsing.Var
import scala.quoted.Quotes

object StageList:
    import Pos.*
    import STerm.*

    def apply[X](xs: Vector[X]): StageList[X] = xs

    opaque type StageList[+X] <: Vector[X] = Vector[X]

    case class Binding[+V, +T](term: T, output: Vector[V])

    case class Basic[+V, +T](prev: Vector[V], next: Vector[V], binding: Option[Binding[V, T]])
    case class VarList[+V](effective: Vector[V], goThrough: Vector[V]):
        def bin: Bin[V] =
            val effectiveBin = Bin.fromElements(effective)
            val goThroughBin = Bin.fromElements(goThrough)
            if goThrough.isEmpty then effectiveBin
            else Bin.Branch(effectiveBin, goThroughBin)

    case class Align[+V, +T](prev: VarList[V], next: VarList[V], binding: Option[Binding[V, T]])
    final case class Adapt[+V, +T, +R](
        prev: VarList[V],
        next: VarList[V],
        adaptation: Adaptation[R] = Vector.empty,
        binding: Option[Binding[V, T]] = None
    )

    trait NoTerm:
        def term = None

    enum Err[+V, +T] extends RuntimeException:
        case UnknownVar(v: V, term: Option[T])
        case UnusedVar(v: V) extends Err[V, T], NoTerm
        case Other(message: String, term: Option[T])

        def term: Option[T]

        override def getMessage = this match
            case UnknownVar(v, t) => s"unknown variable $v"
            case UnusedVar(v)     => s"unused variable $v"
            case Other(m, t)      => s"$m"

        def reportAndAbort(using q: Quotes)(using ev: T <:< q.reflect.Tree): Nothing =
            term match
                case Some(t) => q.reflect.report.errorAndAbort(getMessage, ev(t).pos)
                case None    => q.reflect.report.errorAndAbort(getMessage)

    end Err

    type Vars[+V, +T]     = StageList[Basic[V, T]]
    type Aligned[+V, +T]  = StageList[Align[V, T]]
    type Adapted[V, T, R] = StageList[Adapt[V, T, R]]

    private def toStage[S, T](prev: Vector[S], term: STerm[S, T] & Mid): (Vector[S], Basic[S, T]) =
        term match
            case Assignment(recs, app) => (recs, Basic(prev, app.args, Some(Binding(term.applied, recs))))
            case Application(app)      => (Vector(), Basic(prev, app.args, Some(Binding(term.applied, Vector()))))

    def fromTerms[S, T](
        input: Vector[S],
        terms: Vector[STerm[S, T] & Mid],
        last: STerm[S, T] & End
    ): Vars[S, T] =
        val (lastInput, stages) = terms.mapAccumulate(input)(toStage)
        last match
            case Result(output) => stages :+ Basic(lastInput, output, None)
            case app: Mid       =>
                val (lastestInput, lastStage) = toStage(lastInput, app)
                stages :+ lastStage :+ Basic(lastestInput, Vector(), None)
    end fromTerms

    extension [T, V](list: Vars[V, T])
        def withAdaptation[D](using V: Variable[V, D]): Either[Err[V, T], Adapted[V, T, D]] =
            for
                aligned <- doAlign(list)
                adapted <- aligned.mapErr(histWithOp)
            yield adapted

    private def history[V, T, D](align: Align[V, T])(using
        V: Variable[V, D]
    ): Either[String, Adaptation[D]] =
        val inputBinTree  = align.prev.bin
        val outputBinTree = align.next.bin
        val inputMarks    = inputBinTree.map(V.label)
        import V.describeMagma
        for perms <- inputMarks.adaptation(outputBinTree.map(V.label))
        yield
            val inputResults = inputBinTree.map(V.describe)
            BinRes(inputResults).modAll(perms).history
    end history

    def histWithOp[V, T, D](align: Align[V, T])(using Variable[V, D]): Either[Err[V, T], Adapt[V, T, D]] =
        history(align).fold(
          message => Left(Err.Other(message, align.binding.map(_.term))),
          adaptation => Right(Adapt(align.prev, align.next, adaptation, align.binding))
        )

    end histWithOp

    private def alignSingle[V, T](preserved: Vector[V], list: Basic[V, T])(using
        V: Labeled[V]
    ): Either[Err[V, T], (Vector[V], Align[V, T])] =
        val Basic(prev, next, bind) = list
        val full                    = V.toMap(preserved) ++ V.toMap(prev)
        val unknown                 = V.toMap(next) -- full.keys
        def replenish(v: V)         = V.replenish(v, full(V.label(v)))
        if unknown.isEmpty then
            val keep = (full -- next.view.map(V.label)).view.values.map(replenish).toVector
            Right((keep, Align(VarList(prev, preserved), VarList(next.map(replenish), keep), bind)))
        else Left(Err.UnknownVar(unknown.values.head, bind.map(_.term)))
    end alignSingle

    private def doAlign[V, T](vars: Vars[V, T])(using V: Labeled[V]): Either[Err[V, T], Aligned[V, T]] =
        for
            alignRes         <- vars.mapAccumulateErr(Vector.empty)(alignSingle[V, T])
            (result, aligned) = alignRes
            _                <- Either.cond(result.isEmpty, (), Err.UnusedVar(result.head))
        yield aligned

end StageList
