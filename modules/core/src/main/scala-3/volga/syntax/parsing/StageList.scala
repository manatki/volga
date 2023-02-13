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
    def scanlErr[E, R](f: (Option[T], I, O) => Either[E, R]): Either[E, StageList[T, I, R]] =
        type Res = Vector[Stage[T, R, I]]

        @tailrec def go(elements: List[Stage[T, O, I]], acc: Res, prev: I): Either[E, (I, Res)] =
            elements match
                case Stage(t, i, o) :: rest =>
                    f(Some(t), prev, i) match
                        case Left(err)  => Left(err)
                        case Right(res) => go(rest, acc :+ Stage(t, res, o), o)

                case Nil => Right((prev, acc))

        for
            res             <- go(stages.toList, Vector(), input)
            (last, elements) = res
            last            <- f(None, last, output)
        yield StageList(input, VectorOf(elements), last)
    end scanlErr

    def scanrErr[E, R](f: (Option[T], O, I) => Either[E, R]): Either[E, StageList[T, R, O]] =
        reverse.scanlErr(f).map(_.reverse)

    def reverse: StageList[T, O, I] = StageList(output, stages.map(_.swap).reverse, input)
end StageList

object StageList:
    import Pos.*
    import STerm.*

    type Named[T, S]      = StageList[T, Vector[S], Vector[S]]
    // type Aligned[T, Q, R] = StageList[T, ]
    type Adapted[T, Q, R] = StageList[T, Vector[Q], Out[Q, R]]

    private def toStage[S, T](term: STerm[S, T] & Mid): Stage[T, Vector[S], Vector[S]] = term match
        case Assignment(recs, app) => Stage(app.applied, app.args, recs)
        case Application(app)      => Stage(app.applied, app.args, Vector())

    def fromTerms[S, T](
        input: Vector[S],
        terms: Vector[STerm[S, T] & Mid],
        last: STerm[S, T] & End
    ): Named[T, S] =
        val stages = terms.map(toStage)
        last match
            case Result(output) => StageList(input, VectorOf(stages), output)
            case app: Mid       => StageList(input, VectorOf(stages :+ toStage(app)), Vector())
    end fromTerms

    private def asBinTree[S](names: Vector[S]): Bin[S] =
        names.foldRight[Bin[S]](Bin.Bud)((x, t) => Bin.Branch(Bin.Leaf(x), t))

    def history[S, Q, R: PMagma](input: Vector[Q], output: Vector[S])(
        mark: Q => S,
        res: Q => R
    ): Either[String, Adaptation[R]] =
        val inputBinTree  = asBinTree(input)
        val outputBinTree = asBinTree(output)
        val inputMarks    = inputBinTree.map(mark)
        for perms <- inputMarks.adaptation(outputBinTree)
        yield
            val inputResults = inputBinTree.map(res)
            BinRes(inputResults).modAll(perms).history

    end history

    case class Out[Q, R](elements: Vector[Q], adaptation: Adaptation[R])
    case class Err[T](message: String, term: Option[T])

    extension [Q, T](list: Named[T, Q])
        def withAdaptation[S, R: PMagma](res: Q => R, mark: Q => S): Either[Err[T], Adapted[T, Q, R]] =
            list.scanlErr: (t, prev, cur) =>
                history(prev, cur.map(mark))(mark, res) match
                    case Left(msg)      => Left(Err(msg, t))
                    case Right(history) => Right(Out(cur, history))

end StageList
