package volga.impl

import cats.Monoid
import cats.data.NonEmptyList
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.instances.parallel._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.option._
import cats.syntax.parallel._
import cats.syntax.traverse._
import tofu.optics.chain
import tofu.optics.tags._
import volga.impl.parse.Connect
import volga.solve.BinHistory.HChain
import volga.solve.{BinHistory, BinOp, BinRes, PMagma}
import volga.syntax.comp

import scala.reflect.macros.{TypecheckException, blackbox}
import scala.util.control.NonFatal

class SyntaxMacro(val c: blackbox.Context) extends Unappliers {
  import c.universe._

  sealed trait Mode {
    def P: Type
    def res: Type

    def combineType(xs: List[Type]): Tree = this match {
      case Arrow(_, _) => tq"(..$xs)"
      case SymMon(_, x, _, _) =>
        val resT = xs.reduceLeft((a, b) => appliedType(x, List(a, b)))
        tq"$resT"
    }

    def ident(t: Tree): Tree = q"$compS.ident[$P, $t]"
  }

  case class Arrow(P: Type, res: Type) extends Mode
  case class SymMon(P: Type, x: Type, I: Type, res: Type) extends Mode {
    def smc: Tree = q"$compS.smc[$P, $x, $I]"
  }

  def midTerm  = c.freshName(TermName("mid"))
  val lastTerm = c.freshName(TermName("last"))

  def arr[P: WeakTypeTag, R: WeakTypeTag](body: c.Tree)(vb: Tree): c.Tree =
    generateSyntax(body)(Arrow(weakTypeOf[P].typeConstructor, weakTypeOf[R]))
  def symmon[P: WeakTypeTag, x: WeakTypeTag, I: WeakTypeTag, R: WeakTypeTag](body: c.Tree)(vb: Tree): c.Tree =
    generateSyntax(body)(
      SymMon(weakTypeOf[P].typeConstructor, weakTypeOf[x].typeConstructor, weakTypeOf[I], weakTypeOf[R])
    )

  def namePat(n: TermName): Tree = pq"$n @ _"

  private def constructArrConnect(P: Type, typeMap: Map[TermName, Type])(conn: parse.Connect[TermName]): Tree = {
    val (ins, outs) = conn
    val inpat       = ins.map(names => pq"(..${names.map(namePat)})").reduce((a, b) => pq"($a, $b)")
    val intype      = ins.map(names => tq"(..${names.map(typeMap)})").reduce((a, b) => tq"($a, $b)")
    val outres      = outs.map(names => q"(..$names)").reduce((a, b) => q"($a, $b)")
    val outtype     = outs.map(names => tq"(..${names.map(typeMap)})").reduce((a, b) => tq"($a, $b)")
    val param       = c.freshName(TermName("input"))
    val parin       = q"val $param = $EmptyTree"
    q"$compS.liftf[$P, $intype, $outtype] ($parin => $param match { case $inpat => $outres })"
  }

  private def smc(implicit mode: SymMon): Tree = mode.smc

  private def constructSMCConnect(value: BinRes.History[Type])(implicit mode: SymMon): Option[Tree] =
    value.map(constructSMCConnectStep).reduceOption((a, b) => q"$a.andThen($b)")

  private def constructSMCConnectOrId(value: BinRes.History[Type], t: Type)(implicit mode: SymMon): Tree =
    constructSMCConnect(value).getOrElse(q"$compS.ident[${mode.P}, $t]")

  private def constructSMCConnectStep(value: BinHistory[Type])(implicit mode: SymMon): Tree = {
    import BinOp.{L, R}
    value match {
      case BinHistory.HRotate(L, l, m, r) =>
        c.typecheck(q"$smc.assocl[$l, $m, $r]")
      case BinHistory.HRotate(R, l, m, r) =>
        c.typecheck(q"$smc.assocr[$l, $m, $r]")
      case BinHistory.HSwap(l, r) =>
        c.typecheck(q"$smc.swap[$l, $r]")
      case BinHistory.HSplit(lc, rc) =>
        val HChain(lstart, lend, lhistory) = lc
        val HChain(rstart, rend, rhistory) = rc
        val ltree                          = constructSMCConnectOrId(lhistory, lend)
        val rtee                           = constructSMCConnectOrId(rhistory, rend)
        c.typecheck(q"""$ltree.split($rtee)""")

      case BinHistory.HConsume(L, v) => c.typecheck(q"$smc.lunit[$v]")
      case BinHistory.HConsume(R, v) => c.typecheck(q"$smc.runit[$v]")
      case BinHistory.HGrow(L, v)    => c.typecheck(q"$smc.unitl[$v]")
      case BinHistory.HGrow(R, v)    => c.typecheck(q"$smc.unitr[$v]")
    }
  }

  def getOrPass(
      types: Map[TermName, Type]
  )(a: Assoc[Option[Tree], List[TermName], List[TermName]])(implicit mode: Mode) =
    a.app.getOrElse(mode.ident(mode.combineType(a.in.map(types))))

  def generateSyntax(body: c.Tree)(implicit mode: Mode): c.Tree = {
    val resOpt = body match {
      case q"(..$xs) => $b" =>
        b match {
          case q"{..$ls}" =>
            val xns = xs.collect { case ValDef(_, name, VarTyp(tt), _) => (name, tt) }.toList
            val ps  = parse.collectBody(ls.toList, xns.map(_._1), matchElem)
            val (outTypes, parsed) = ps.fold({
              case (to, x) => c.abort(to.fold(c.enclosingPosition)(_.pos), x + to.toString)
            }, identity)
            val typeMap: Map[TermName, Type] = ((lastTerm -> mode.res) +: xns ++: outTypes).toMap
            val withLaterUse                 = parse.addLaterUse(parsed)
            val flow                         = withLaterUse.app.map(_.map(getOrPass(typeMap)).reduce((a, b) => q"($a.split($b))"))
            mode match {
              case Arrow(p, r) =>
                val connects             = parse.inOuts(withLaterUse).map(constructArrConnect(p, typeMap))
                val res                  = parse.alternate(connects, flow).reduce((x, y) => q"($x.andThen($y))")
                val Assoc(body, in, out) = withLaterUse

                res.some
              case symMode @ SymMon(p, x, i, r) =>
                val (reused, unused) = parse.preventReuse(parsed)
                c.info(c.enclosingPosition, parsed.toString, true)
                reused.foreach { case (t, n) => c.error(t.pos, s"variable $n is used second time") }
                unused.foreach {
                  case (t, n) => c.error(t.fold(c.enclosingPosition)(_.pos), s"variable $n is not used")
                }
                if (reused.nonEmpty || unused.nonEmpty) c.abort(c.enclosingPosition, "error in variable use")

                implicit val xsymbol: ComposeSymbol = ComposeSymbol(x)

                val connectUpdater =
                  chain.to[List[Connect[ComInfo]]](parse.inOuts(withLaterUse)) >
                    every >
                    every >
                    every >
                    every end

                val connects =
                  connectUpdater
                    .update(n => ComInfo(Some(n), typeMap(n)))
                    .parTraverse(parse.binTransfers[ComInfo])
                    .fold(reportConnectErrors, identity)
                    .map(
                      res => constructSMCConnect(res.history.map(_.map(_.typ)))(symMode)
                    )

                val res = parse
                  .alternateOpt(connects, flow.map(_.some))
                  .reduce((a, b) => q"$a.andThen($b)")

                res.some
            }
          case _ => none
        }
      case _ => none
    }
    val res = resOpt.getOrElse(c.abort(c.enclosingPosition, "unexpected syntax"))

    res
  }

  def reportConnectErrors(xs: NonEmptyList[String]): Nothing = c.abort(c.enclosingPosition, "there were errors")

  def matchElem[A](t: Tree, last: Boolean): (List[(TermName, Type)], ParseElem[Tree, TermName]) = t match {
    case ValDef(_, name, VarTyp(tt), ArrSyn(smth, args)) => (List(name -> tt), ParseElem.Single(args, name, smth))
    case ValDef(_, name, VarTyp(tt), VarElem(v, i))      => (List(name -> tt), ParseElem.MultiAdd(v, name, i - 1))
    case ValDef(mods, name, tt, Match(ArrSyn(smth, args), _)) if mods.hasFlag(Flag.SYNTHETIC) =>
      val arity = tt.tpe match {
        case TypeRef(_, _, xs) => xs.length
        case _                 => 0
      }
      (List(), ParseElem.MultiStart(args, name, smth, arity))
    case q"${Break()}"          => (List(), ParseElem.Split)
    case q"(..${Names(names)})" => (List(), ParseElem.Result(names))
    case ArrSyn(smth, args) =>
      val name = if (last) lastTerm else midTerm
      c.info(smth.pos, args.toString(), false)
      (List(), ParseElem.MultiStart(args, name, smth, 0))
    case l => (List(), ParseElem.Other(l))
  }

  final case class ComposeSymbol(t: Type)

  final case class ComInfo(name: Option[TermName], typ: Type)

  object ComInfo {
    implicit def pmagma(implicit compose: ComposeSymbol): PMagma[ComInfo] = new PMagma[ComInfo] {
      val empty: ComInfo = ComInfo(None, NoType)

      def combine(x: ComInfo, y: ComInfo): ComInfo =
        ComInfo(None, appliedType(compose.t, List(x.typ, y.typ)))
    }
  }
}

trait Unappliers {
  val c: blackbox.Context

  import c.universe._

  val compS = reify(comp).tree.symbol
  val Vsym  = typeOf[comp.V[Unit]].typeConstructor.typeSymbol
  object Syntax {
    def unapply(tree: Tree): Boolean = tree.symbol == compS
  }

  object Break {
    def unapply(tree: Tree): Boolean =
      tree match {
        case q"${Syntax()}.----" => true
        case _                   => false
      }
  }

  object Names {
    def unapply(t: List[Tree]): Option[List[TermName]] =
      t.traverse { case q"${a: TermName}" => Some(a); case _ => None }
  }

  object ArrSyn {
    def unapply(tree: Tree): Option[(Tree, List[TermName])] =
      tree match {
        case q"${Syntax()}.ArrSyn[..$_]($smth)(..$_).apply[..$_](..${Names(names)})(..$_)" => Some((smth, names))
        case q"${Syntax()}.SMCSyn[..$_]($smth)(..$_).apply[..$_](..${Names(names)})(..$_)" => Some((smth, names))
        case q"$tree: $_"                                                                  => unapply(tree)
        case _                                                                             => None
      }
  }

  object VarElem {
    val sub = "_(\\d+)".r
    def unapply(tree: Tree): Option[(TermName, Int)] =
      tree match {
        case q"${n: TermName}.${TermName(sub(i))}" =>
          try Some((n, i.toInt))
          catch {
            case NonFatal(_) => None
          }
        case _ => None
      }
  }

  object VarTyp {
    def unapply(t: Tree): Option[Type] =
      t match {
        case tq"${tt: Type}" =>
          tt match {
            case TypeRef(t, Vsym, List(tp)) if t.termSymbol == compS => Some(tp)
            case _                                                   => None
          }

        case _ => None
      }
  }

  object DebugLst {
    def unapply(t: List[Any]): Some[String] = Some(t.mkString("(", ",", ")"))
  }
}
