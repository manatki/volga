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

// yes, functorial argument is I, not O
case class Stage[+T, +I, +O](term: T, input: I, output: O) derives Traverse:
    def swap: Stage[T, O, I] = Stage(term, output, input)

object Stage:
    given [T, O]: Traverse[Stage[T, _, O]] with
        extension [A](fa: Stage[T, A, O])
            def traverse[F[+_]: Monoidal, B](f: A => F[B]): F[Stage[T, B, O]] =
                f(fa.input).map(b => fa.copy(input = b))

// strangely enough input and output are swapped in stages, so
final case class StageList[+T, +I, +O](input: I, stages: VectorOf[Stage[T, _, I], O], output: O) derives Traverse:

    def scanlErr[E, S, X, Y](
        start: I => Either[E, (S, X)],
        mid: (T, O, I, S) => Either[E, (S, X, Y)],
        end: (S, O) => Either[E, (S, Y)]
    ): Either[E, (S, StageList[T, X, Y])] =
        type Res = Vector[Stage[T, Y, X]]

        @tailrec def go(elements: List[Stage[T, O, I]], acc: Res, prev: S): Either[E, (S, Res)] =
            elements match
                case Stage(t, i, o) :: rest =>
                    mid(t, i, o, prev) match
                        case Left(err)           => Left(err)
                        case Right((next, x, y)) => go(rest, acc :+ Stage(t, y, x), next)

                case Nil => Right((prev, acc))

        for
            initInp            <- start(input)
            (init, newInput)    = initInp
            traversed          <- go(stages.toList, Vector(), init)
            (preLast, elements) = traversed
            lastOut            <- end(preLast, output)
            (last, newOutput)   = lastOut
        yield (last, StageList(newInput, VectorOf(elements), newOutput))
    end scanlErr

    def scanl1Err[E, R](f: (Option[T], I, O) => Either[E, R]): Either[E, StageList[T, I, R]] =
        scanlErr()

    def scanrErr[E, R](f: (Option[T], O, I) => Either[E, R]): Either[E, StageList[T, R, O]] =
        reverse.scanl1Err(f).map(_.reverse)

    def reverse: StageList[T, O, I] = StageList(output, VectorOf(stages.map(_.swap).reverse), input)
end StageList

object StageList:
    import Pos.*
    import STerm.*

    type Vars[T, V]       = StageList[T, Vector[V], Vector[V]]
    type Aligned[T, V]    = StageList[T, Align[V], Align[V]]
    type Adapted[T, V, R] = StageList[T, Align[V], Out[V, R]]

    private def toStage[S, T](term: STerm[S, T] & Mid): Stage[T, Vector[S], Vector[S]] = term match
        case Assignment(recs, app) => Stage(app.applied, app.args, recs)
        case Application(app)      => Stage(app.applied, app.args, Vector())

    def fromTerms[S, T](
        input: Vector[S],
        terms: Vector[STerm[S, T] & Mid],
        last: STerm[S, T] & End
    ): Vars[T, S] =
        val stages = terms.map(toStage)
        last match
            case Result(output) => StageList(input, VectorOf(stages), output)
            case app: Mid       => StageList(input, VectorOf(stages :+ toStage(app)), Vector())
    end fromTerms

    private def vectorAsBinTree[S](names: Vector[S]): Bin[S] =
        names.foldRight[Bin[S]](Bin.Bud)((x, t) => Bin.Branch(Bin.Leaf(x), t))

    private def asBinTree[S](names: Align[S]): Bin[S] =
        val operational = vectorAsBinTree(names.operational)
        if names.goThrough.isEmpty then operational
        else
            val goThrough = vectorAsBinTree(names.goThrough)
            Bin.Branch(operational, goThrough)

    private def history[V, D](input: Align[V], output: Align[V])(using
        V: Variable[V, D]
    ): Either[String, Adaptation[D]] =
        val inputBinTree  = asBinTree(input)
        val outputBinTree = asBinTree(output)
        val inputMarks    = inputBinTree.map(V.label)
        import V.describeMagma
        for perms <- inputMarks.adaptation(outputBinTree.map(V.label))
        yield
            val inputResults = inputBinTree.map(V.describe)
            BinRes(inputResults).modAll(perms).history

    end history

    // private def carryNext(prev: Vector[V])

    final case class Out[V, D](aligned: Align[V], adaptation: Adaptation[D])
    final case class Align[V](operational: Vector[V], goThrough: Vector[V])
    final case class Err[T](message: String, term: Option[T])

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
    )(using V: Variable[V, D]): Either[Err[T], Adapted[T, V, D]] =
        aligned.scanl1Err: (t, prev, cur) =>
            history(prev, cur) match
                case Left(msg)      => Left(Err(msg, t))
                case Right(history) => Right(Out(cur, history))
end StageList
