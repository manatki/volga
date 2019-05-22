package volga.pres

import cats.arrow.Arrow
import cats.{Applicative, Monad, StackSafeMonad}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.arrow._
import cats.syntax.compose._

import scala.Function.tupled


sealed trait ConsoleM[X]

object ConsoleM {
  case class Pure[A](x: A) extends ConsoleM[A]
  case class Bind[A, B](x: ConsoleM[A], fab: A => ConsoleM[B]) extends ConsoleM[B]
  case object GetLine extends ConsoleM[String]
  case class PutLine(s: String) extends ConsoleM[Unit]

  implicit val monad: Monad[ConsoleM] = new StackSafeMonad[ConsoleM] {
    def pure[A](x: A): ConsoleM[A] = Pure(x)
    def flatMap[A, B](fa: ConsoleM[A])(f: A => ConsoleM[B]): ConsoleM[B] = Bind(fa, f)
  }

  val getLine: ConsoleM[String] = GetLine

  def echo2: ConsoleM[Unit] = for (x <- getLine; y <- getLine) yield x + y

  def countGets[A](cm: ConsoleM[A]): Int = cm match {
    case Pure(_) => 0
    case GetLine => 1
    case PutLine(_) => 0
    case Bind(m, f) => countGets(m) + (??? : Int)
  }
}


sealed trait ConsoleA[X]

object ConsoleA {
  case class Pure[A](x: A) extends ConsoleA[A]
  case class Ap[A, B](f: ConsoleA[A => B], x: ConsoleA[A]) extends ConsoleA[B]
  case object GetLine extends ConsoleA[String]
  case class PutLine(s: String) extends ConsoleA[Unit]

  implicit val applicative: Applicative[ConsoleA] = new Applicative[ConsoleA] {
    def pure[A](x: A): ConsoleA[A] = Pure(x)
    def ap[A, B](ff: ConsoleA[A => B])(fa: ConsoleA[A]): ConsoleA[B] = Ap(ff, fa)
  }

  def countGets[X](ca: ConsoleA[X]): Int = ca match {
    case Pure(_) => 0
    case GetLine => 1
    case PutLine(_) => 0
    case Ap(f, x) => countGets(f) + countGets(x)
  }

  def echo2: ConsoleA[Unit] = ???
}

sealed trait ConsoleArr[X, Y]

object ConsoleArr extends App{
  case class Lift[A, B](f: A => B) extends ConsoleArr[A, B]
  case class AndThen[A, B, C](start: ConsoleArr[A, B], next: ConsoleArr[B, C]) extends ConsoleArr[A, C]
  case class Split[A, B, C, D](first: ConsoleArr[A, B], second: ConsoleArr[C, D]) extends ConsoleArr[(A, C), (B, D)]
  case object GetLine extends ConsoleArr[Unit, String]
  case object PutLine extends ConsoleArr[String, Unit]

  implicit val arrow: Arrow[ConsoleArr] = new Arrow[ConsoleArr] {
    def lift[A, B](f: A => B): ConsoleArr[A, B] = Lift(f)
    def first[A, B, C](fa: ConsoleArr[A, B]): ConsoleArr[(A, C), (B, C)] = Split(fa, id)
    def compose[A, B, C](f: ConsoleArr[B, C], g: ConsoleArr[A, B]): ConsoleArr[A, C] = AndThen(g, f)
  }

  val getLine: ConsoleArr[Unit, String] = GetLine
  val putLine: ConsoleArr[String, Unit] = PutLine

  def concat: ConsoleArr[(String, String), String] = Lift(tupled(_ + _))

  def echo2: ConsoleArr[Unit, Unit] =
    (getLine &&& getLine) >>> concat >>> PutLine

  def countGets[X, Y](carr: ConsoleArr[X, Y]): Int = carr match {
    case Lift(_) => 0
    case AndThen(start, next) => countGets(start) + countGets(next)
    case Split(first, second) => countGets(first) + countGets(second)
    case GetLine => 1
    case PutLine => 0
  }

  import volga.syntax._

  def echo2s: ConsoleArr[Unit, Unit] = arr[ConsoleArr] { unit =>
    val s1 = getLine(unit)
    val s2 = getLine(unit)
    val s = concat(s1, s2)
    val res = putLine(s)

    res
  }
}
