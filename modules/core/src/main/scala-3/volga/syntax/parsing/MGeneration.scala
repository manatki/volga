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

        def show = s"${hom.show} : ${Type.show[In]} --> ${Type.show[Out]}"
    end TypedHomC

    type TypedHom = TypedHomC[?, ?]

    def generate(terms: StageList.Adapted[Var[q.type], Tree, MndType[q, TypeRepr]]): Expr[Any] =
        val exp = terms.view.map(generateAdaptTerm).reduce(andThen).hom
        report.info(s"generated: ${exp.show} of type ${exp.asTerm.tpe.dealias.widen.show}")

        exp

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
                andThen(preamble, mainPart)
        end match
    end generateAdaptTerm

    private def generateAdaptation(adapt: Adaptation[TypeRepr], inT: => TypeRepr): TypedHom =
        adapt.view.map(generateForBinHistory).reduceOption(andThen).getOrElse(identity(inT))

    private def generateForBinHistory(hist: BinHistory[TypeRepr]): TypedHom = hist match
        case BinHistory.HRotate(side, l, m, r) => hrotate(side, l, m, r)
        case BinHistory.HSwap(l, r)            => swap(l, r)
        case BinHistory.HSplit(left, right)    => splitChain(left, right)
        case BinHistory.HConsume(side, v)      => consume(side, v)
        case BinHistory.HGrow(side, v)         => grow(side, v)

    private def andThen[A, B, B1, C](x: TypedHomC[A, B], y: TypedHomC[B1, C]): TypedHomC[A, C] =
        given Type[A]            = x.I
        given Type[B]            = x.O
        given Type[C]            = y.O
        val yexpr: Expr[H[B, C]] = y.hom.asExprOf[H[B, C]]
        TypedHomC[A, C](
          hom = '{ $sym.compose[A, B, C]($yexpr, ${ x.hom })(using ${ x.obI }, ${ x.obO }, ${ y.obO }) },
          obI = x.obI,
          obO = y.obO
        )
    end andThen

    private def swap(x: TypeRepr, y: TypeRepr): TypedHom =
        x.asType match
            case '[x] =>
                y.asType match
                    case '[y] =>
                        val obI = summonOb[x]
                        val obO = summonOb[y]
                        TypedHomC(
                          hom = '{ $sym.braiding[x, y](using $obI, $obO) },
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
                          hom = '{ $sym.leftUnit[x](using $obX).from },
                          obI = obX,
                          obO = '{ $sym.tensorOb($sym.unitOb, $obX) }
                        )
                    case R =>
                        TypedHomC(
                          hom = '{ $sym.rightUnit[x](using $obX).from },
                          obI = obX,
                          obO = '{ $sym.tensorOb($obX, $sym.unitOb) }
                        )
                end match
    end grow

    private def splitChain(left: HChain[TypeRepr], right: HChain[TypeRepr]): TypedHom =
        val leftTerm  = generateAdaptation(left.history, left.start)
        val rightTerm = generateAdaptation(right.history, right.start)
        split(leftTerm, rightTerm)

    private def split[A, B, C, D](left: TypedHomC[A, B], right: TypedHomC[C, D]): TypedHomC[A x C, B x D] =
        given Type[A] = left.I
        given Type[B] = left.O
        given Type[C] = right.I
        given Type[D] = right.O
        TypedHomC[A x C, B x D](
          hom = '{
              $sym.tensor[A, B, C, D](${ left.hom }, ${ right.hom })(using
                ${ left.obI },
                ${ left.obO },
                ${ right.obI },
                ${ right.obO }
              )
          },
          obI = '{ $sym.tensorOb[A, C](using ${ left.obI }, ${ right.obI }) },
          obO = '{ $sym.tensorOb[B, D](using ${ left.obO }, ${ right.obO }) }
        )
    end split

    private def hrotate(side: Side, a: TypeRepr, b: TypeRepr, c: TypeRepr): TypedHom =
        a.asType match
            case '[a] =>
                b.asType match
                    case '[b] =>
                        c.asType match
                            case '[c] =>
                                val obA = summonOb[a]
                                val obB = summonOb[b]
                                val obC = summonOb[c]
                                side match
                                    case L =>
                                        TypedHomC(
                                          hom = '{ $sym.assocLeft[a, b, c](using $obA, $obB, $obC) },
                                          obI = '{ $sym.tensorOb($obA, $sym.tensorOb($obB, $obC)) },
                                          obO = '{ $sym.tensorOb($sym.tensorOb($obA, $obB), $obC) }
                                        )
                                    case R =>
                                        TypedHomC(
                                          hom = '{ $sym.assocRight[a, b, c](using $obA, $obB, $obC) },
                                          obI = '{ $sym.tensorOb($sym.tensorOb($obA, $obB), $obC) },
                                          obO = '{ $sym.tensorOb($obA, $sym.tensorOb($obB, $obC)) }
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
