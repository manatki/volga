package volga
package syntax.parsing

import scala.quoted.Quotes
import volga.syntax.solve.StageList
import scala.quoted.Expr
import scala.quoted.Type
import volga.syntax.solve.StageList.VarList
import volga.syntax.solve.Adaptation
import volga.syntax.solve.BinHistory
import volga.syntax.solve.binop.Side
import scala.compiletime.summonInline

final class MGeneration[H[_, _]: Type, U[_]: Type, q <: Quotes & Singleton](sym: Expr[SymmetricCat[H, U]])(using
    val q: q
)(using
    typing: MonadicTyping[q.type]
) extends Aliases[H, U]:
    import q.reflect.*

    class TypedHomC[In, Out](
        val hom: Expr[H[In, Out]],
        val obI: Expr[U[tags.Obj[In]]],
        val obO: Expr[U[tags.Obj[Out]]]
    )(using val I: Type[In], val O: Type[Out]):
        type I = In
        type O = Out
    end TypedHomC

    type TypedHom = TypedHomC[?, ?]

    def generate(terms: StageList.Adapted[Var[q.type], Tree, MndType[q, TypeRepr]]): Tree =
        '{ () }.asTerm
    end generate

    private def genType(v: VarList[Var[q.type]]): TypeRepr =
        if v.goThrough.isEmpty then joinType(v.effective)
        else tensorType(joinType(v.effective), joinType(v.goThrough))

    private def joinType(v: Vector[Var[q.type]]): TypeRepr = v.view.map(_.requireTyp).reduce(tensorType)

    private def tensorType(x: TypeRepr, y: TypeRepr): TypeRepr = typing.tensor.appliedTo(List(x, y))

    private def generateAdaptTerm(
        adapt: StageList.Adapt[Var[q.type], Tree, TypeRepr]
    ): TypedHom =
        val preamble = generateAdaptation(adapt.adaptation, genType(adapt.prev))
        ???

    private def generateAdaptation(adapt: Adaptation[TypeRepr], inT: => TypeRepr): TypedHom =
        adapt.view.map(generateForBinHistory).reduceOption(compose).getOrElse(identity(inT))

    private def generateForBinHistory(hist: BinHistory[TypeRepr]): TypedHom = hist match
        case BinHistory.HRotate(side, l, m, r) => ???
        case BinHistory.HSwap(l, r)            => swap(l, r)
        case BinHistory.HSplit(left, right)    => ???
        case BinHistory.HConsume(side, v)      => ???
        case BinHistory.HGrow(side, v)         => ???

    private def compose(x: TypedHom, y: TypedHom): TypedHom =
        type A = x.I
        type B = y.I
        type C = y.O
        given Type[A]            = x.I
        given Type[B]            = y.I
        given Type[C]            = y.O
        val xexpr                = x.hom.asExprOf[H[A, B]]
        val yexpr: Expr[H[B, C]] = y.hom
        TypedHomC[A, C](
          hom = '{ $sym.compose[A, B, C](${ y.hom }, $xexpr)(using ${ x.obI }, ${ y.obI }, ${ y.obO }) },
          obI = x.obI,
          obO = y.obO
        )
    end compose

    private def swap(x: TypeRepr, y: TypeRepr): TypedHom =
        x.asType match
            case '[x] =>
                y.asType match
                    case '[y] =>
                        val obI: Expr[U[tags.Obj[x]]] = '{ summonInline }
                        val obO: Expr[U[tags.Obj[y]]] = '{ summonInline }
                        TypedHomC(
                          hom = '{ $sym.braiding(using $obI, $obO) },
                          obI = '{ $sym.tensorOb($obI, $obO) },
                          obO = '{ $sym.tensorOb($obO, $obI) }
                        )

    private def hrotate(side: Side, x: TypeRepr, y: TypeRepr, z: TypeRepr): TypedHom = ???

    private def identity(x: TypeRepr): TypedHom =
        x.asType match
            case '[x] =>
                val obX: Expr[U[tags.Obj[x]]] = '{ summonInline }
                TypedHomC(
                  hom = '{ $sym.identity(using $obX) },
                  obI = obX,
                  obO = obX
                )

end MGeneration
