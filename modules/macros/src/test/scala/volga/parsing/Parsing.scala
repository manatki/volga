package volga
package parsing

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.regex.Pattern

import cats.Id
import cats.data.{EitherNel, NonEmptyList}
import cats.instances.either._
import cats.instances.parallel._
import cats.syntax.either._
import cats.syntax.option._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import volga.data.Dikleisli
import volga.syntax.cat._
import volga.syntax.comp._
import volga.syntax.symmon._

import scala.util.Try

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

  val time: Parsing[((Int, Int), Int), LocalTime] =
    Dikleisli { dmy: ((Int, Int), Int) =>
      val ((h, m), s) = dmy
      Try(LocalTime.of(h, m, s)).toEither.leftMap(_.getMessage).toEitherNel
    }(d => ((d.getHour, d.getMinute), d.getSecond): Id[((Int, Int), Int)])

  val dateTime: Parsing[(LocalDate, LocalTime), LocalDateTime] =
    Dikleisli { dmy: (LocalDate, LocalTime) =>
      val (d, t) = dmy
      Try(LocalDateTime.of(d, t)).toEither.leftMap(_.getMessage).toEitherNel
    }(d => (d.toLocalDate, d.toLocalTime): Id[(LocalDate, LocalTime)])

  val parsing = symon[Parsing, (*, *), Unit]

  val parseDate: Parsing[String, LocalDate] = parsing { (s: V[String]) =>
    val (dayStr, monthYear) = sep(".")(s)
    val (monthStr, yearStr) = sep(".")(monthYear)
    ----
    val year: V[Int]  = readInt(yearStr)
    val month: V[Int] = readInt(monthStr)
    val day: V[Int]   = readInt(dayStr)

    date(day, month, year)
  }

  val parseTime: Parsing[String, LocalTime] = parsing { (s: V[String]) =>
    val (hourStr, rest1)       = sep(":")(s)
    val (minuteStr, secondStr) = sep(":")(rest1)
    ----
    val hour: V[Int]   = readInt(hourStr)
    val minute: V[Int] = readInt(minuteStr)
    val second: V[Int] = readInt(secondStr)

    time(hour, minute, second)
  }

  val parseDateTime: Parsing[String, LocalDateTime] = parsing { (s: V[String]) =>
    val (dayStr, rest1)        = sep(".")(s)
    val (monthStr, rest2)      = sep(".")(rest1)
    val (yearStr, rest3)       = sep(" ")(rest2)
    val (hourStr, rest4)       = sep(":")(rest3)
    val (minuteStr, secondStr) = sep(":")(rest4)
    ----
    val year   = readInt(yearStr)
    val month  = readInt(monthStr)
    val day    = readInt(dayStr)
    val hour   = readInt(hourStr)
    val minute = readInt(minuteStr)
    val second = readInt(secondStr)
    ----
    val d = date(day, month, year)
    val t = time(hour, minute, second)

    dateTime(d, t)
  }



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

object ParsingByHand {
  import Parsing._
  val parseDate1: Parsing[String, LocalDate] = sep(".")
    .andThen(ident[Parsing, String].split(sep(".")))
    .andThen(parsingSMC.assocl[String, String, String])
    .andThen(readInt.split(readInt).split(readInt))
    .andThen(
      parsingSMC
        .assocr[Int, Int, Int]
        .andThen(parsingSMC.assocl[Int, Int, Int])
    )
    .andThen(date)

  val parseDate2: Parsing[String, LocalDate] = sep(".")
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

  val parseDate3: Parsing[String, LocalDate] = sep(".")
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
        .swap[Int, Int]
        .split(ident[Parsing, Int])
        .andThen(parsingSMC.assocr[Int, Int, Int])
        .andThen(ident[Parsing, Int].split(parsingSMC.swap[Int, Int]))
        .andThen(parsingSMC.assocl[Int, Int, Int])
        .andThen(parsingSMC.swap[Int, Int].split(ident[Parsing, Int]))
    )
    .andThen(date)


  val parseDateAndTimeManual =
    sep(".")
      .andThen(ident[Parsing, String].split(sep(".")))
      .andThen(
        parsingSMC.assocl[String, String, String].andThen(parsingSMC.swap[String, String].split(ident[Parsing, String]))
      )
      .andThen(ident[Parsing, (String, String)].split(sep(" ")))
      .andThen(
        ident[Parsing, (String, String)]
          .split(ident[Parsing, (String, String)])
          .andThen(parsingSMC.assocr[String, String, (String, String)])
          .andThen(
            ident[Parsing, String]
              .split(
                parsingSMC
                  .assocl[String, String, String]
                  .andThen(
                    parsingSMC
                      .swap[String, String]
                      .split(ident[Parsing, String])
                  )
                  .andThen(parsingSMC.assocr[String, String, String])
              )
          )
          .andThen(parsingSMC.assocl[String, String, (String, String)])
          .andThen(parsingSMC.swap[String, String].split(ident[Parsing, (String, String)]))
          .andThen(parsingSMC.assocl[(String, String), String, String])
      )
      .andThen(ident[Parsing, Tuple2[Tuple2[String, String], String]].split(sep(":")))
      .andThen(
        parsingSMC
          .assocr[(String, String), String, (String, String)]
          .andThen(parsingSMC.assocr[String, String, (String, (String, String))])
          .andThen(
            ident[Parsing, String]
              .split(
                parsingSMC
                  .assocl[String, String, (String, String)]
                  .andThen(ident[Parsing, (String, String)].split(ident[Parsing, (String, String)]))
                  .andThen(parsingSMC.assocr[String, String, (String, String)])
                  .andThen(
                    ident[Parsing, String]
                      .split(
                        parsingSMC
                          .assocl[String, String, String]
                          .andThen(
                            parsingSMC
                              .swap[String, String]
                              .split(ident[Parsing, String])
                          )
                          .andThen(parsingSMC.assocr[String, String, String])
                      )
                  )
                  .andThen(parsingSMC.assocl[String, String, (String, String)])
                  .andThen(parsingSMC.swap[String, String].split(ident[Parsing, (String, String)]))
                  .andThen(parsingSMC.assocr[String, String, (String, String)])
              )
          )
          .andThen(parsingSMC.assocl[String, String, (String, (String, String))])
          .andThen(parsingSMC.assocl[(String, String), String, (String, String)])
          .andThen(parsingSMC.assocl[((String, String), String), String, String])
      )
      .andThen(ident[Parsing, (((String, String), String), String)].split(sep(":")))
      .andThen(
        parsingSMC
          .assocr[((String, String), String), String, (String, String)]
          .andThen(parsingSMC.assocr[(String, String), String, (String, (String, String))])
          .andThen(parsingSMC.assocr[String, String, (String, (String, (String, String)))])
          .andThen(
            ident[Parsing, String]
              .split(
                parsingSMC
                  .assocl[String, String, (String, (String, String))]
                  .andThen(
                    parsingSMC
                      .swap[String, String]
                      .split(ident[Parsing, (String, (String, String))])
                  )
                  .andThen(parsingSMC.assocr[String, String, (String, (String, String))])
                  .andThen(
                    ident[Parsing, String]
                      .split(
                        parsingSMC
                          .assocl[String, String, (String, String)]
                          .andThen(
                            parsingSMC
                              .swap[String, String]
                              .split(ident[Parsing, (String, String)])
                          )
                          .andThen(parsingSMC.assocr[String, String, (String, String)])
                      )
                  )
                  .andThen(parsingSMC.assocl[String, String, (String, (String, String))])
                  .andThen(ident[Parsing, (String, String)].split(ident[Parsing, (String, (String, String))]))
                  .andThen(parsingSMC.assocr[String, String, (String, (String, String))])
              )
          )
          .andThen(parsingSMC.assocl[String, String, (String, (String, (String, String)))])
          .andThen(parsingSMC.assocl[(String, String), String, (String, (String, String))])
          .andThen(parsingSMC.assocl[((String, String), String), String, (String, String)])
          .andThen(parsingSMC.assocl[(((String, String), String), String), String, String])
      )
      .andThen(
        readInt
          .split(readInt)
          .split(readInt)
          .split(readInt)
          .split(readInt)
          .split(readInt)
      )
      .andThen(
        parsingSMC
          .assocr[(((Int, Int), Int), Int), Int, Int]
          .andThen(parsingSMC.assocr[((Int, Int), Int), Int, (Int, Int)])
          .andThen(parsingSMC.assocr[(Int, Int), Int, (Int, (Int, Int))])
          .andThen(parsingSMC.swap[Int, Int].split(ident[Parsing, (Int, (Int, (Int, Int)))]))
          .andThen(parsingSMC.assocr[Int, Int, (Int, (Int, (Int, Int)))])
          .andThen(
            ident[Parsing, Int]
              .split(
                parsingSMC
                  .assocl[Int, Int, (Int, (Int, Int))]
                  .andThen(parsingSMC.swap[Int, Int].split(ident[Parsing, (Int, (Int, Int))]))
                  .andThen(parsingSMC.assocr[Int, Int, (Int, (Int, Int))])
              )
          )
          .andThen(parsingSMC.assocl[Int, Int, (Int, (Int, (Int, Int)))])
          .andThen(parsingSMC.swap[Int, Int].split(ident[Parsing, (Int, (Int, (Int, Int)))]))
          .andThen(parsingSMC.assocr[Int, Int, (Int, (Int, (Int, Int)))])
          .andThen(
            ident[Parsing, Int]
              .split(
                ident[Parsing, Int]
                  .split(
                    ident[Parsing, Int]
                      .split(parsingSMC.assocl[Int, Int, Int])
                  )
              )
          )
          .andThen(parsingSMC.assocl[Int, Int, (Int, ((Int, Int), Int))])
          .andThen(parsingSMC.assocl[(Int, Int), Int, ((Int, Int), Int)])
      )
      .andThen(date.split(time))
      .andThen(dateTime)
}
