package volga
package parsing

import java.time.LocalDate
import java.util.regex.Pattern

import cats.data.{EitherNel, NonEmptyList}
import cats.{Applicative, Id, Monad, Parallel}
import cats.instances.parallel._
import cats.syntax.parallel._
import cats.syntax.either._
import cats.syntax.option._
import cats.instances.tuple._
import cats.instances.either._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tofu.syntax.monadic._
import volga.syntax.comp

import scala.util.Try
import scala.util.matching.Regex

trait Dikleisli[F[_], G[_], A, B] {
  def to(a: A): F[B]
  def from(b: B): G[A]
}

object MPar extends LowPriorMPar {
  type TC[F[_]] <: Parallel[F]

  implicit def instanceByParalel[F[_]](implicit F: Parallel[F]): TC[F] = F.asInstanceOf[TC[F]]
}

trait LowPriorMPar { self: MPar.type =>
  implicit def instanceByMonad[F[_]: Monad]: TC[F] = Parallel.identity[F].asInstanceOf[TC[F]]
}

object Dikleisli {
  def apply[F[_], G[_], A, B](f: A => F[B])(g: B => G[A]): Dikleisli[F, G, A, B] = new Dikleisli[F, G, A, B] {
    def to(a: A): F[B]   = f(a)
    def from(b: B): G[A] = g(b)
  }

  def lift[F[_], G[_]] = new LiftApplied[F, G](true)

  class LiftApplied[F[_], G[_]](private val __ : Boolean) extends AnyVal {
    def apply[A, B](f: A => B)(g: B => A)(implicit F: Applicative[F], G: Applicative[G]): Dikleisli[F, G, A, B] =
      new Dikleisli[F, G, A, B] {
        def to(a: A): F[B]   = f(a).pure[F]
        def from(b: B): G[A] = g(b).pure[G]
      }
  }

  implicit def instance[F[_]: MPar.TC: Monad, G[_]: MPar.TC: Monad]: DikleisliSymon[F, G] = new DikleisliSymon[F, G]

  class DikleisliSymon[F[_]: MPar.TC: Monad, G[_]: MPar.TC: Monad] extends Symon[Dikleisli[F, G, *, *], (*, *), Unit] {
    @inline private def lifted[A, B](f: A => B)(g: B => A): Dikleisli[F, G, A, B] = lift[F, G](f)(g)

    def swap[A, B] = lifted((_: (A, B)).swap)(_.swap)

    def lunit[A] = lifted((_: (Unit, A))._2)(((), _))

    def unitl[A] = lifted(((), _: A))(_._2)

    def assocl[A, B, C] =
      lifted[(A, (B, C)), ((A, B), C)] { case (a, (b, c)) => ((a, b), c) } { case ((a, b), c) => (a, (b, c)) }

    def tensor[A, B, C, D](f: Dikleisli[F, G, A, B], g: Dikleisli[F, G, C, D]) = new Dikleisli[F, G, (A, C), (B, D)] {
      def to(a: (A, C)): F[(B, D)] = a.parBitraverse(f.to, g.to)

      def from(b: (B, D)): G[(A, C)] = b.parBitraverse(f.from, g.from)
    }

    def id[A]: Dikleisli[F, G, A, A] = lifted[A, A](a => a)(a => a)

    def compose[A, B, C](f: Dikleisli[F, G, B, C], g: Dikleisli[F, G, A, B]): Dikleisli[F, G, A, C] =
      new Dikleisli[F, G, A, C] {
        def to(a: A): F[C] = g.to(a).flatMap(f.to)

        def from(b: C): G[A] = f.from(b).flatMap(g.from)
      }
  }
}

object Parsing {
  implicit val parsingSMC: Symon[Parsing, (*, *), Unit] = Dikleisli.instance

  type Parsing[A, B] = Dikleisli[EitherNel[String, *], Id, A, B]

  def lift[A, B] = Dikleisli.lift[Either[String, *], Id]

  def eq[A](value: A): Parsing[A, Unit] =
    Dikleisli((a: A) => if (a == value) ().rightNel else s"expected '$value' got $a".leftNel)(_ => value: Id[A])

  def sep(sep: String): Parsing[String, (String, String)] =
    Dikleisli(
      (s: String) =>
        s.split(Pattern.quote(sep), 2) match {
          case Array(first, rest) => (first, rest).rightNel
          case _                  => s"$s does not contain '$sep'".leftNel
        }
    ) { case (s1, s2) => s"$s1$sep$s2" }

  val readInt: Parsing[String, Int] =
    Dikleisli((s: String) => s.toIntOption.toRightNel(s"$s is not an int"))((_: Int).toString: Id[String])

  val date: Parsing[((Int, Int), Int), LocalDate] =
    Dikleisli { dmy: ((Int, Int), Int) =>
      val ((d, m), y) = dmy
      Try(LocalDate.of(y, m, d)).toEither.leftMap(_.getMessage).toEitherNel
    }(d => ((d.getDayOfMonth, d.getMonthValue), d.getYear): Id[((Int, Int), Int)])

  import volga.syntax.comp._
  import volga.syntax.cat._
  import volga.syntax.symmon._

  val parsing = symon[Parsing, Tuple2, Unit]

  val parseDate1: Parsing[String, LocalDate] = parsing { (s: V[String]) =>
    val (dayStr: V[String], monthYear: V[String]) = SMCSyn(sep(".")).apply(s)
    val (monthStr: V[String], yearStr: V[String]) = SMCSyn(sep(".")).apply(monthYear)
    ----
    val year: V[Int] = SMCSyn(readInt).apply(yearStr)
    val month: V[Int] = SMCSyn(readInt).apply(monthStr)
    val day: V[Int] = SMCSyn(readInt).apply(dayStr)

    SMCSyn(date).apply(day, month, year)
  }

  val parseDate0: Parsing[String, LocalDate] = sep(".")
    .andThen(ident[Parsing, String].split(sep(".")))
    .andThen(parsingSMC.assocl[String, String, String])
    .andThen(readInt.split(readInt).split(readInt))
    .andThen(
      parsingSMC
        .assocr[Int, Int, Int]
        .andThen(parsingSMC.assocl[Int, Int, Int])
    )
    .andThen(date)

  val parseDate: Parsing[String, LocalDate] = sep(".")
    .andThen(ident[Parsing, String].split(sep(".")))
    .andThen(
      parsingSMC
        .assocl[String, String, String]
        .andThen(parsingSMC.swap[String, String].split(ident[Parsing, String]))
        .andThen(parsingSMC.assocr[String, String, String])
        .andThen(ident[Parsing, String].split(parsingSMC.swap[String, String]))
        .andThen(parsingSMC.assocl[String, String, String])
        .andThen(parsingSMC.swap[String, String].split(ident[Parsing, String]))
    )
    .andThen(readInt.split(readInt).split(readInt))
    .andThen(
      parsingSMC
        .assocr[Int, Int, Int]
        .andThen(parsingSMC.assocl[Int, Int, Int])
        .andThen(parsingSMC.swap[Int, Int].split(ident[Parsing, Int]))
        .andThen(parsingSMC.assocr[Int, Int, Int])
        .andThen(ident[Parsing, Int].split(parsingSMC.swap[Int, Int]))
        .andThen(parsingSMC.assocl[Int, Int, Int])
        .andThen(parsingSMC.swap[Int, Int].split(ident[Parsing, Int]))
    )
    .andThen(date)

  def main(args: Array[String]): Unit = {
    val today = parseDate.from(LocalDate.now())
    println(today)
    println(parseDate.to(today))
    println(parseDate.to("31.11.2019"))
    println(parseDate.to("31.112019"))
    println(parseDate.to("a.b.c"))
  }
}

class ParsingSuite extends AnyFlatSpec with Matchers {
  import Parsing.parseDate

  "parseDate" should "pretty print date" in { parseDate.from(LocalDate.of(2019, 12, 8)) should be("8.12.2019") }

  it should "parse date" in { parseDate.to("8.12.2019") should be(Right(LocalDate.of(2019, 12, 8))) }

  it should "report logic error" in { parseDate.to("31.11.2019") should be("Invalid date 'NOVEMBER 31'".leftNel) }

  it should "report split error" in { parseDate.to("31.112019") should be("112019 does not contain '.'".leftNel) }

  it should "report errors in parallel" in {
    parseDate.to("a.b.c") should be(
      Left(
        NonEmptyList.of(
          "a is not an int",
          "b is not an int",
          "c is not an int",
        )
      )
    )
  }
}
