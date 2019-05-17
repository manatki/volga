package volga

import cats.instances.list._
import cats.instances.option._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.syntax.option._

import scala.reflect.macros.blackbox
import scala.util.control.NonFatal

class SyntaxMacro(val c: blackbox.Context) extends Unappliers {

  import c.universe._

  sealed trait Mode {
    def P: Type
  }

  case class Arrow(P: Type)                    extends Mode
  case class SymMon(P: Type, x: Type, I: Type) extends Mode

  def arr[P: WeakTypeTag](body: c.Tree)(vb: Tree): c.Tree = generateSyntax(body, Arrow(weakTypeOf[P].typeConstructor))
  def symmon[P: WeakTypeTag, x: WeakTypeTag, I: WeakTypeTag](body: c.Tree)(vb: Tree): c.Tree =
    generateSyntax(body, SymMon(weakTypeOf[P].typeConstructor, weakTypeOf[x].typeConstructor, weakTypeOf[I].typeConstructor))

  private def constructArrConnect(P: Type, typeMap: Map[TermName, Type])(conn: parse.Connect[TermName]): Tree = {
    val (ins, outs) = conn
    val inpat       = ins.map(names => pq"(..${names.map(n => pq"$n @ _")})").reduce((a, b) => pq"($a, $b)")
    val intype      = ins.map(names => tq"(..${names.map(typeMap)})").reduce((a, b) => tq"($a, $b)")
    val outres      = outs.map(names => q"(..$names)").reduce((a, b) => q"($a, $b)")
    val outtype     = outs.map(names => tq"(..${names.map(typeMap)})").reduce((a, b) => tq"($a, $b)")
    val param       = c.freshName[TermName]("input")
    val parin       = q"val $param = $EmptyTree"
    q"$syntSym.liftf[$P, $intype, $outtype] ($parin => $param match { case $inpat => $outres })"
  }

  def getOrPass(types: Map[TermName, Type], P: Type)(a: Assoc[Option[Tree], List[TermName], List[TermName]]) =
    a.app.getOrElse(q"$syntSym.ident[$P, (..${a.in.map(types)})]")

  def generateSyntax(body: c.Tree, mode: Mode): c.Tree = {
    val (resOpt, debug: List[Any]) = body match {
      case q"(..$xs) => $b" =>
        b match {
          case q"{..$ls}" =>
            val lm                 = ls.toList.map(matchElem)
            val xns                = xs.collect { case ValDef(_, name, VarTyp(tt), _) => (name, tt) }.toList
            val ps                 = parse.collectBody(ls.toList, xns.map(_._1), matchElem)
            val (outTypes, parsed) = ps.fold({ case (to, x) => c.abort(to.fold(c.enclosingPosition)(_.pos), x) }, identity)
            val typeMap            = (xns ++ outTypes).toMap
            mode match {
              case Arrow(p) =>
                val withLaterUse         = parse.addLaterUse(parsed)
                val connects             = parse.inOuts(withLaterUse).map(constructArrConnect(p, typeMap))
                val flow                 = withLaterUse.app.map(_.map(getOrPass(typeMap, p)).reduce((a, b) => q"($a.split($b))"))
                val res                  = parse.alternate(connects, flow).reduce((x, y) => q"($x.andThen($y))")
                val Assoc(body, in, out) = withLaterUse

                val bodspl = body
                  .map(_.map { case Assoc(b, ins, outs) => s"$outs <- ${b.getOrElse("<<REUSE>>")} -< $ins" })
                  .intercalate(List("<<BREAK>>"))
                (res.some, outTypes :: res :: in :: out :: bodspl)
              case SymMon(p, x, i) =>
                val reused = parse.preventReuse(parsed)
                reused.foreach { case (t, n) => c.error(t.pos, s"variable $n is used second time") }
                c.abort(c.enclosingPosition, " monoidal syntax not implemented")
            }
          case _ => (none, List(xs, b))
        }
      case _ => (none, List(body))
    }
    val sss = debug.map(e => s"-----------\n$e\n").mkString
    val res = resOpt.getOrElse(q"null")
    c.info(c.enclosingPosition, res.toString(), true)
    q"""
       println($sss)
       $res
      """
  }

  def matchElem[A](t: Tree): (List[(TermName, Type)], ParseElem[Tree, TermName]) = t match {
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
    case l                      => (List(), ParseElem.Other(l))
  }
}

trait Unappliers {
  val c: blackbox.Context
  import c.universe._

  val syntSym = reify(syntax).tree.symbol
  val Vsym    = typeOf[syntax.V[Unit]].typeConstructor.typeSymbol
  object Syntax {
    def unapply(tree: Tree): Boolean = tree.symbol == syntSym
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
        case q"${Syntax()}.ArrSyn[..$_]($smth).apply[..$_](..${Names(names)})(..$_)" => Some((smth, names))
        case q"$tree: $_"                                                            => unapply(tree)
        case _                                                                       => None
      }
  }

  object VarElem {
    val sub = "_(\\d+)".r
    def unapply(tree: Tree): Option[(TermName, Int)] =
      tree match {
        case q"${n: TermName}.${TermName(sub(i))}" =>
          try Some((n, i.toInt))
          catch { case NonFatal(_) => None }
        case _ => None
      }
  }

  object VarTyp {
    def unapply(t: Tree): Option[Type] =
      t match {
        case tq"${tt: Type}" =>
          tt match {
            case TypeRef(t, Vsym, List(tp)) if t.termSymbol == syntSym => Some(tp)
            case _                                                     => None
          }

        case _ => None
      }
  }
}
