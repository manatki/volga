package volga.syntax.parsing

import volga.functors.*
import volga.syntax.solve.Permutations
import volga.syntax.solve.Bin
import volga.syntax.solve.BinOp
import volga.syntax.solve.History
import volga.syntax.solve.BinRes
import volga.syntax.solve.PMagma
import scala.annotation.tailrec

object VectorOf:
    opaque type VectorOf[+F[+_], +A] <: Vector[F[A]] = Vector[F[A]]

    def apply[F[+_], A](fa: Vector[F[A]]): VectorOf[F, A] = fa

    given [F[+_]: Functor]: Functor[VectorOf[F, _]] = summon[Functor[Vector]].composeFunctor[F]

    given [F[+_]: Traverse]: Traverse[VectorOf[F, _]] = summon[Traverse[Vector]].composeTraverse[F]

    given [F[+_], A]: Conversion[Vector[F[A]], VectorOf[F, A]] = x => x
end VectorOf

type VectorOf[+F[+_], +A] = VectorOf.VectorOf[F, A]

case class App[+S, +T](applied: T, args: Vector[S]) derives Traverse

package Pos:
    trait Mid
    trait End
    trait Tupling

enum STerm[+S, +T] derives Traverse:
    case Assignment[+S, +T](receivers: Vector[S], application: App[S, T]) extends STerm[S, T], Pos.Mid
    case Tupled[+S, +T](receiver: S, arity: Int, application: App[S, T])  extends STerm[S, T], Pos.Tupling
    case Untupling[+S](src: S, tgt: S, index: Int)                        extends STerm[S, Nothing], Pos.Tupling
    case Result[+S](results: Vector[S])                                   extends STerm[S, Nothing], Pos.End
    case Application[+S, +T](applied: App[S, T])                          extends STerm[S, T], Pos.Mid, Pos.End

// yes, functorial argument is I, not O
case class Stage[+T, +O, +I](term: T, input: I, output: O) derives Traverse

// strangely enough input and output are swapped in stages, so
case class StageList[+T, +I, +O](input: I, stages: VectorOf[Stage[T, I, _], O], output: O) derives Traverse

object StageList:
    import Pos.*
    import STerm.*

    private def toStage[S, T](term: STerm[S, T] & Mid): Stage[T, Vector[S], Vector[S]] = term match
        case Assignment(recs, app) => Stage(app.applied, app.args, recs)
        case Application(app)      => Stage(app.applied, app.args, Vector())

    def fromTerms[S, T](
        input: Vector[S],
        terms: Vector[STerm[S, T] & Mid],
        last: STerm[S, T] & End
    ): StageList[T, Vector[S], Vector[S]] =
        val stages = terms.map(toStage)
        last match
            case Result(output) => StageList(input, stages, output)
            case app: Mid       => StageList(input, stages :+ toStage(app), Vector())
    end fromTerms

    private def asBinTree[S](names: Vector[S]): Bin[S] =
        names.foldRight[Bin[S]](Bin.Bud)((x, t) => Bin.Branch(Bin.Leaf(x), t))

    def history[S, Q, R: PMagma](input: Vector[Q], output: Vector[S])(
        getMark: Q => S,
        getRes: Q => R
    ): Either[String, History[R]] =
        val inputBinTree  = asBinTree(input)
        val outputBinTree = asBinTree(output)
        val inputMarks    = inputBinTree.map(getMark)
        for perms <- inputMarks.adaptation(outputBinTree)
        yield
            val inputResults = inputBinTree.map(getRes)
            BinRes(inputResults).modAll(perms).history

    end history

    case class Out[Q, R](elements: Vector[Q], history: History[R])
    case class Err[T](message: String, term: Option[T])

    extension [Q, S, T](list: StageList[T, Vector[Q], Vector[Q]])
        def fillHistories[R: PMagma](res: Q => R, mark: Q => S): Either[Err[T], StageList[T, Out[Q, R], Vector[Q]]] =
            type Res = Vector[Out[Q, R]]
            def out(t: Option[T], prev: Vector[Q], cur: Vector[Q]): Either[Err[T], Out[Q, R]] =
                history(prev, cur.map(mark))(mark, res) match
                    case Left(msg)      => Left(Err(msg, t))
                    case Right(history) => Right(Out(cur, history))

            @tailrec def go(
                elements: List[Stage[T, Vector[Q], Vector[Q]]],
                acc: Res,
                prev: Vector[Q]
            ): Either[Err[T], (Vector[Q], Res)] =
                elements match
                    case Stage(t, i, o) :: rest =>
                        out(Some(t), prev, i) match
                            case Left(err)  => Left(err)
                            case Right(res) => go(rest, acc :+ res, o)

                    case Nil => Right((prev, acc))

            for
                res             <- go(list.stages.toList, Vector(), list.input)
                (last, elements) = res
                last            <- out(None, last, list.output)
            yield ???

end StageList
