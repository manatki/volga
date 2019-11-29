package volga.pres

import cats.arrow.Arrow
import cats.{Applicative, Monad, StackSafeMonad}
import cats.syntax.flatMap._
import cats.syntax.functor._
import volga.syntax.arr._

import scala.Function.tupled
import scala.io.StdIn

sealed trait ConsoleM[X]

object ConsoleM {
  case class Pure[A](x: A)                                     extends ConsoleM[A]
  case class Bind[A, B](x: ConsoleM[A], fab: A => ConsoleM[B]) extends ConsoleM[B]
  case object GetLine                                          extends ConsoleM[String]
  case class PutLine(s: String)                                extends ConsoleM[Unit]

  implicit val monad: Monad[ConsoleM] =
    new StackSafeMonad[ConsoleM] {
      def pure[A](x: A): ConsoleM[A]                                       = Pure(x)
      def flatMap[A, B](fa: ConsoleM[A])(f: A => ConsoleM[B]): ConsoleM[B] = Bind(fa, f)
    }

  val getLine: ConsoleM[String] = GetLine

  def echo2: ConsoleM[Unit] =
    for (x <- getLine; y <- getLine) yield x + y

  def countGets[A](cm: ConsoleM[A]): Int = cm match {
    case Pure(_)    => 0
    case GetLine    => 1
    case PutLine(_) => 0
    case Bind(m, f) => countGets(m) + (??? : Int)
  }
}

sealed trait ConsoleA[X]

object ConsoleA {
  case class Pure[A](x: A)                                 extends ConsoleA[A]
  case class Ap[A, B](f: ConsoleA[A => B], x: ConsoleA[A]) extends ConsoleA[B]
  case object GetLine                                      extends ConsoleA[String]
  case class PutLine(s: String)                            extends ConsoleA[Unit]

  implicit val applicative: Applicative[ConsoleA] =
    new Applicative[ConsoleA] {
      def pure[A](x: A): ConsoleA[A]                                   = Pure(x)
      def ap[A, B](ff: ConsoleA[A => B])(fa: ConsoleA[A]): ConsoleA[B] = Ap(ff, fa)
    }

  def countGets[X](ca: ConsoleA[X]): Int = ca match {
    case Pure(_)    => 0
    case GetLine    => 1
    case PutLine(_) => 0
    case Ap(f, x)   => countGets(f) + countGets(x)
  }

  def echo2: ConsoleA[Unit] = ???
}

sealed trait ConsoleArr[X, Y]

object ConsoleArr extends App {
  case class Lift[A, B](f: A => B)                                                extends ConsoleArr[A, B]
  case class AndThen[A, B, C](start: ConsoleArr[A, B], next: ConsoleArr[B, C])    extends ConsoleArr[A, C]
  case class Split[A, B, C, D](first: ConsoleArr[A, B], second: ConsoleArr[C, D]) extends ConsoleArr[(A, C), (B, D)]
  case object GetLine                                                             extends ConsoleArr[Unit, String]
  case object PutLine                                                             extends ConsoleArr[String, Unit]

  implicit val arrow: volga.Arr[ConsoleArr] =
    new volga.Arr[ConsoleArr] {
      def lift[A, B](f: A => B): ConsoleArr[A, B] = Lift(f)


      def split[A, B, C, D](f: ConsoleArr[A, C], g: ConsoleArr[B, D]): ConsoleArr[(A, B), (C, D)] = Split(f, g)

      def compose[A, B, C](f: ConsoleArr[B, C], g: ConsoleArr[A, B]): ConsoleArr[A, C] = AndThen(g, f)
    }

  val getLine: ConsoleArr[Unit, String] = GetLine
  val putLine: ConsoleArr[String, Unit] = PutLine

  val concat: ConsoleArr[(String, String), String] = Lift(tupled(_ + _))
  val show: ConsoleArr[Int, String]                = Lift(_.toString)
  val plus: ConsoleArr[(Int, Int), Int]            = Lift(tupled(_ + _))
  val divMod: ConsoleArr[(Int, Int), (Int, Int)]   = Lift { case (x, y) => (x / y, x % y) }

  def echo2: ConsoleArr[Unit, Unit] =
    (getLine &&& getLine) >>> concat >>> putLine

  def countGets[X, Y](carr: ConsoleArr[X, Y]): Int =
    carr match {
      case Lift(_) => 0
      case AndThen(start, next) =>
        countGets(start) + countGets(next)
      case Split(first, second) =>
        countGets(first) + countGets(second)
      case GetLine => 1
      case PutLine => 0
    }

  import volga.syntax.comp._
  import volga.syntax.comp

  def echo2s: ConsoleArr[Unit, Unit] = arr[ConsoleArr] { () =>
    val s1 = getLine()
    val s2 = getLine()
    val s  = concat(s1, s2)
    putLine(s)
  }

  val console = arr[ConsoleArr]

  val foo: ConsoleArr[(Int, Int), (Int, String)] = console { (x, y) =>
    val xs = show(x)
    val ys = show(y)
    putLine(xs)
    putLine(ys)
    val (u, v) = divMod(x, y)
    val z      = plus(u, v)
    val zs     = show(z)
    val s      = concat(xs, ys)
    val t      = concat(zs, s)
    putLine(zs)
    (v, t)
  }

  run(foo)(11, 3)

  liftf[volga.pres.ConsoleArr, (Int, Int), (((Int, Int), Int), Int)] {
    case (x, y) => (((x, y), x), y)
  }.andThen(ident[volga.pres.ConsoleArr, (Int, Int)]
      .split(show)
      .split(show))
    .andThen(comp.liftf[volga.pres.ConsoleArr, (((Int, Int), String), String), ((String, Int, Int, String), String)] {
      case (((x, y), xs), ys) => ((ys, x, y, xs), xs)
    })
    .andThen(ident[volga.pres.ConsoleArr, (String, Int, Int, String)].split(putLine))
    .andThen(comp
      .liftf[volga.pres.ConsoleArr, ((String, Int, Int, String), Unit), (((String, String), String), (Int, Int))] {
        case ((ys, x, y, xs), ()) => (((xs, ys), ys), (x, y))
      })
    .andThen(ident[volga.pres.ConsoleArr, (String, String)]
      .split(putLine)
      .split(plus))
    .andThen(comp.liftf[volga.pres.ConsoleArr, (((String, String), Unit), Int), ((String, String), Int)] {
      case (((xs, ys), ()), z) => ((xs, ys), z)
    })
    .andThen(ident[volga.pres.ConsoleArr, (String, String)]
      .split(show))
    .andThen(comp.liftf[volga.pres.ConsoleArr, ((String, String), String), (String, (String, String))] {
      case ((xs, ys), zs) => (zs, (xs, ys))
    })
    .andThen(ident[volga.pres.ConsoleArr, String].split(concat))
    .andThen(comp.liftf[volga.pres.ConsoleArr, (String, String), (((String, String), String), String)] {
      case (zs, s) => (((zs, s), zs), s)
    })
    .andThen(concat.split(putLine).split(putLine))
    .andThen(comp.liftf[volga.pres.ConsoleArr, ((String, Unit), Unit), String] {
      case ((t, ()), ()) => t
    })
    .andThen(putLine)

  //  println(echo2s)

  def run[X, Y](a: ConsoleArr[X, Y]): X => Y =
    a match {
      case Lift(f) => f
      case Split(f, g) => { (x: X) =>
        val (u, v) = x.asInstanceOf[(Any, Any)]
        (run(f)(u), run(g)(v))
      }
      case PutLine => {
        case x: String => println(x)
      }
      case GetLine => { _ =>
        StdIn.readLine()
      }
      case AndThen(f, g) =>
        x =>
          run(g)(run(f)(x))

    }

}
