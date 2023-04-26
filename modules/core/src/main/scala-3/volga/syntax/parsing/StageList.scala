package volga.syntax.parsing

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

final case class StageList[+T, +B](ops: Vector[T], bindings: Vector[B]) derives Traverse

object StageList:
    import Pos.*
    import STerm.*

    case class VarList[+V](prev: Vector[V], next: Vector[V])
    final case class Align[+V](prev: Vector[V], next: Vector[V], goThrough: Vector[V])
    final case class Out[+V, D](prev: Vector[V], next: Vector[V], goThrough: Vector[V], adaptation: Adaptation[D])
    final case class Err[T](message: String, term: Option[T])

    type Vars[+T, +V]     = StageList[T, VarList[V]]
    type Aligned[T, V]    = StageList[T, Align[V]]
    type Adapted[T, V, R] = StageList[T, Out[V, R]]

    private def toStage[S, T](prev: Vector[S], term: STerm[S, T] & Mid): (Vector[S], VarList[S]) =
        term match
            case Assignment(recs, app) => (recs, VarList(prev, app.args))
            case Application(app)      => (Vector(), VarList(prev, app.args))

    def fromTerms[S, T](
        input: Vector[S],
        terms: Vector[STerm[S, T] & Mid],
        last: STerm[S, T] & End
    ): Vars[T, S] =
        val (lastInput, stages) = terms.mapAccumulate(input)(toStage)
        val ops                 = terms.map(_.applied)
        last match
            case Result(output) => StageList(ops, stages :+ VarList(lastInput, output))
            case app: Mid       =>
                val (lastestInput, lastStage) = toStage(lastInput, app)
                StageList(ops, stages :+ lastStage :+ VarList(lastestInput, Vector()))
    end fromTerms

    private def vectorAsBinTree[S](names: Vector[S]): Bin[S] =
        names.foldRight[Bin[S]](Bin.Bud)((x, t) => Bin.Branch(Bin.Leaf(x), t))

    private def history[V, D](align: Align[V])(using
        V: Variable[V, D]
    ): Either[String, Adaptation[D]] =
        val inputBinTree  = vectorAsBinTree(align.prev)
        val outputBinTree = vectorAsBinTree(align.next)
        val inputMarks    = inputBinTree.map(V.label)
        import V.describeMagma
        for perms <- inputMarks.adaptation(outputBinTree.map(V.label))
        yield
            val inputResults = inputBinTree.map(V.describe)
            BinRes(inputResults).modAll(perms).history
    end history

    def histWithOp[V, D, T](align: Align[V], op: Option[T])(using Variable[V, D]): Either[Err[T], Out[V, D]] =
        history(align).fold(
          message => Left(Err(message, op)),
          adaptation => Right(Out(align.prev, align.next, align.goThrough, adaptation))
        )

    extension [T, V](list: Vars[T, V])
        def withAdaptation[D](using V: Variable[V, D]): Either[Err[T], Adapted[T, V, D]] =
            for
                aligned <- doAlign(list)
                adapted <- doAdaptation(aligned)
            yield adapted

    private def doAlign[T, V](vars: Vars[T, V])(using V: Labeled[V]): Either[Err[T], Aligned[T, V]] =
        // val idented = vars.scanrErr: (t, prev, cur) =>
        ???

    private def doAdaptation[T, V, D](
        aligned: Aligned[T, V]
    )(using Variable[V, D]): Either[Err[T], Adapted[T, V, D]] =
        for
            basicOps <- aligned.bindings.zip(aligned.ops).mapErr((a, o) => histWithOp(a, Some(o)))
            last     <- histWithOp(aligned.bindings.last, None)
        yield aligned.copy(bindings = basicOps :+ last)
end StageList
