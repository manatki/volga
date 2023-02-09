package volga.syntax.parsing
import volga.functors.*
import volga.syntax.solve.Permutations
import volga.syntax.solve.Bin
import volga.syntax.solve.BinOp

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

case class Stage[+T, +I, +O](term: T, input: I, output: O) derives Traverse

case class StageList[+T, +I, +O](stages: VectorOf[Stage[T, I, _], O], input: O, output: I) derives Traverse

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
            case Result(output) => StageList(stages, input, output)
            case app: Mid       => StageList(stages :+ toStage(app), input, Vector())
    end fromTerms

    private def asBinTree[S](names: Vector[S]): Bin[S]                                         =
        names.foldRight[Bin[S]](Bin.Bud)((x, t) => Bin.Branch(Bin.Leaf(x), t))
        
    def makePermutation[S](input: Vector[S], output: Vector[S]): Either[String, Vector[BinOp]] =
        val inputBinTree  = asBinTree(input)
        val outputBinTree = asBinTree(output)
        inputBinTree.adaptation(outputBinTree)
end StageList
