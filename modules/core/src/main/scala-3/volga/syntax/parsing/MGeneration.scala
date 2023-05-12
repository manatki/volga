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
import volga.syntax.solve.binop.L
import volga.syntax.solve.binop.R
import volga.syntax.solve.BinHistory.HChain

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

    def generate(terms: StageList.Adapted[Var[q.type], Tree, MndType[q, TypeRepr]]): Expr[Any] =
        terms.view.map(generateAdaptTerm).reduce(compose).hom

    private def genType(v: VarList[Var[q.type]]): TypeRepr =
        if v.goThrough.isEmpty then joinType(v.effective)
        else tensorType(joinType(v.effective), joinType(v.goThrough))

    private def joinType(v: Vector[Var[q.type]]): TypeRepr =
        v.view.map(_.requireTyp).reduceOption(tensorType).getOrElse(typing.one)

    private def tensorType(x: TypeRepr, y: TypeRepr): TypeRepr = typing.tensor.appliedTo(List(x, y))

    private def generateAdaptTerm(
        adapt: StageList.Adapt[Var[q.type], Tree, TypeRepr]
    ): TypedHom =
        val preamble = generateAdaptation(adapt.adaptation, genType(adapt.prev))
        adapt.binding match
            case None       => preamble
            case Some(bind) =>
                val termHom  = treeToHom(bind.term, adapt.next.effective, bind.output)
                val mainPart =
                    if adapt.next.goThrough.nonEmpty
                    then
                        val goThrough = identity(joinType(adapt.next.goThrough))
                        split(termHom, goThrough)
                    else termHom
                compose(preamble, mainPart)
        end match
    end generateAdaptTerm

    private def generateAdaptation(adapt: Adaptation[TypeRepr], inT: => TypeRepr): TypedHom =
        adapt.view.map(generateForBinHistory).reduceOption(compose).getOrElse(identity(inT))

    private def generateForBinHistory(hist: BinHistory[TypeRepr]): TypedHom = hist match
        case BinHistory.HRotate(side, l, m, r) => hrotate(side, l, m, r)
        case BinHistory.HSwap(l, r)            => swap(l, r)
        case BinHistory.HSplit(left, right)    => splitChain(left, right)
        case BinHistory.HConsume(side, v)      => consume(side, v)
        case BinHistory.HGrow(side, v)         => grow(side, v)

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

    private def summonOb[X: Type]: Expr[U[tags.Obj[X]]] = '{ summonInline[U[tags.Obj[X]]] }

    private def identity(x: TypeRepr): TypedHom =
        x.asType match
            case '[x] =>
                val obX = summonOb[x]
                TypedHomC(
                  hom = '{ $sym.identity(using $obX) },
                  obI = obX,
                  obO = obX
                )

    private def consume(side: Side, x: TypeRepr): TypedHom =
        x.asType match
            case '[x] =>
                val obX = summonOb[x]
                side match
                    case L =>
                        TypedHomC(
                          hom = '{ $sym.leftUnit(using $obX).to },
                          obI = '{ $sym.tensorOb($sym.unitOb, $obX) },
                          obO = obX
                        )
                    case R =>
                        TypedHomC(
                          hom = '{ $sym.rightUnit(using $obX).to },
                          obI = '{ $sym.tensorOb($obX, $sym.unitOb) },
                          obO = obX
                        )
                end match
    end consume

    private def grow(side: Side, x: TypeRepr): TypedHom =
        x.asType match
            case '[x] =>
                val obX = summonOb[x]
                side match
                    case L =>
                        TypedHomC(
                          hom = '{ $sym.leftUnit(using $obX).from },
                          obI = obX,
                          obO = '{ $sym.tensorOb($sym.unitOb, $obX) }
                        )
                    case R =>
                        TypedHomC(
                          hom = '{ $sym.rightUnit(using $obX).from },
                          obI = obX,
                          obO = '{ $sym.tensorOb($obX, $sym.unitOb) }
                        )
                end match
    end grow

    private def splitChain(left: HChain[TypeRepr], right: HChain[TypeRepr]): TypedHom =
        val leftTerm  = generateAdaptation(left.history, left.start)
        val rightTerm = generateAdaptation(right.history, right.start)
        split(leftTerm, rightTerm)

    private def split(left: TypedHom, right: TypedHom): TypedHom =
        import left.given
        import right.given
        TypedHomC(
          hom = '{
              $sym.tensor(${ left.hom }, ${ right.hom })(using
                ${ left.obI },
                ${ left.obO },
                ${ right.obI },
                ${ right.obO }
              )
          },
          obI = '{ $sym.tensorOb(${ left.obI }, ${ right.obI }) },
          obO = '{ $sym.tensorOb(${ left.obO }, ${ right.obO }) }
        )
    end split

    private def hrotate(side: Side, x: TypeRepr, y: TypeRepr, z: TypeRepr): TypedHom =
        x.asType match
            case '[x] =>
                y.asType match
                    case '[y] =>
                        z.asType match
                            case '[z] =>
                                val obX = summonOb[x]
                                val obY = summonOb[y]
                                val obZ = summonOb[z]
                                side match
                                    case L =>
                                        TypedHomC(
                                          hom = '{ $sym.assocLeft(using $obX, $obY, $obZ) },
                                          obI = '{ $sym.tensorOb($obX, $sym.tensorOb($obY, $obZ)) },
                                          obO = '{ $sym.tensorOb($sym.tensorOb($obX, $obY), $obZ) }
                                        )
                                    case R =>
                                        TypedHomC(
                                          hom = '{ $sym.assocRight(using $obX, $obY, $obZ) },
                                          obI = '{ $sym.tensorOb($sym.tensorOb($obX, $obY), $obZ) },
                                          obO = '{ $sym.tensorOb($obX, $sym.tensorOb($obY, $obZ)) }
                                        )
                                end match
    end hrotate

    def validate(adapted: StageList.Adapted[Var[q.type], Tree, MndType[q, TypeRepr]]): Unit =
        for
            adapt            <- adapted
            (position, vars) <- Array(
                                  "next.effective" -> adapt.next.effective,
                                  "next.goThrough" -> adapt.next.goThrough,
                                  "prev.effective" -> adapt.prev.effective,
                                  "prev.goThrough" -> adapt.prev.goThrough,
                                  "bindingRes"     -> adapt.binding.map(_.output).getOrElse(Vector())
                                )
            v                <- vars
            if v.typ.isEmpty
        do report.error(s"untyped variable $v at $position")

    private def treeToHom(tree: Tree, inVars: Vector[Var[q.type]], outVars: Vector[Var[q.type]]): TypedHom =
        joinType(inVars).asType match
            case '[i] =>
                joinType(outVars).asType match
                    case '[o] =>
                        TypedHomC(
                          hom = tree.asExpr.asExprOf[H[i, o]],
                          obI = summonOb[i],
                          obO = summonOb[o]
                        )

end MGeneration
