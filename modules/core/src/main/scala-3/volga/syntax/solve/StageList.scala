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

object StageList:
    import Pos.*
    import STerm.*

    def apply[X](xs: Vector[X]): StageList[X] = xs

    opaque type StageList[+X] <: Vector[X] = Vector[X]

    case class VarList[+V, +T](prev: Vector[V], next: Vector[V], binding: Option[T])
    final case class Align[+V, +T](prev: Vector[V], next: Vector[V], goThrough: Vector[V], binding: Option[T])
    final case class Out[+V, +T, +R](
        prev: Vector[V],
        next: Vector[V],
        goThrough: Vector[V],
        adaptation: Adaptation[R] = Vector.empty,
        binding: Option[T] = None
    )

    enum Err[+V, +T]:
        case UnknownVar(v: V, t: Option[T])
        case UnusedVar(v: V)
        case Other(message: String, term: Option[T])

    type Vars[+V, +T]     = StageList[VarList[V, T]]
    type Aligned[+V, +T]  = StageList[Align[V, T]]
    type Adapted[V, T, R] = StageList[Out[V, T, R]]

    private def toStage[S, T](prev: Vector[S], term: STerm[S, T] & Mid): (Vector[S], VarList[S, T]) =
        term match
            case Assignment(recs, app) => (recs, VarList(prev, app.args, Some(term.applied)))
            case Application(app)      => (Vector(), VarList(prev, app.args, Some(term.applied)))

    def fromTerms[S, T](
        input: Vector[S],
        terms: Vector[STerm[S, T] & Mid],
        last: STerm[S, T] & End
    ): Vars[S, T] =
        val (lastInput, stages) = terms.mapAccumulate(input)(toStage)
        last match
            case Result(output) => stages :+ VarList(lastInput, output, None)
            case app: Mid       =>
                val (lastestInput, lastStage) = toStage(lastInput, app)
                stages :+ lastStage :+ VarList(lastestInput, Vector(), None)
    end fromTerms

    private def history[V, T, D](align: Align[V, T])(using
        V: Variable[V, D]
    ): Either[String, Adaptation[D]] =
        val inputBinTree  = Bin.fromElements(align.prev)
        val outputBinTree = Bin.fromElements(align.next)
        val inputMarks    = inputBinTree.map(V.label)
        import V.describeMagma
        for perms <- inputMarks.adaptation(outputBinTree.map(V.label))
        yield
            val inputResults = inputBinTree.map(V.describe)
            BinRes(inputResults).modAll(perms).history
    end history

    def histWithOp[V, T, D](align: Align[V, T])(using Variable[V, D]): Either[Err[V, T], Out[V, T, D]] =
        history(align).fold(
          message => Left(Err.Other(message, align.binding)),
          adaptation => Right(Out(align.prev, align.next, align.goThrough, adaptation, align.binding))
        )

    end histWithOp

    private def alignSingle[V, T](preserved: Vector[V], list: VarList[V, T])(using
        V: Labeled[V]
    ): Either[Err[V, T], (Vector[V], Align[V, T])] =
        val VarList(prev, next, bind) = list
        val full                      = V.toMap(preserved) ++ V.toMap(prev)
        val unknown                   = V.toMap(next) -- full.keys
        if unknown.isEmpty then
            val keep = (full -- next.view.map(V.label)).values.toVector
            Right((keep, Align(prev ++ preserved, next, keep, bind)))
        else Left(Err.UnknownVar(unknown.values.head, bind))
    end alignSingle

    private def doAlign[V, T](vars: Vars[V, T])(using V: Labeled[V]): Either[Err[V, T], Aligned[V, T]] =
        for
            alignRes         <- vars.mapAccumulateErr(Vector.empty)(alignSingle[V, T])
            (result, aligned) = alignRes
            _                <- Either.cond(result.isEmpty, (), Err.UnusedVar(result.head))
        yield aligned

    extension [T, V](list: Vars[V, T])
        def withAdaptation[D](using V: Variable[V, D]): Either[Err[V, T], Adapted[V, T, D]] =
            for
                aligned <- doAlign(list)
                adapted <- aligned.mapErr(histWithOp)
            yield adapted

end StageList
