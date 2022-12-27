package volga
import cats.{Eval, Monad}
import cats.data.Kleisli
import volga.syntax.comp._
import volga.syntax.all._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._

import scala.io.StdIn

object Console1 {
  trait ConsoleA[->[_, _]] {
    def readLn: Unit -> String
    def printLn: String -> Unit
  }

  type Run[A, B] = Kleisli[Eval, A, B]

  implicit object kleisliConsole extends ConsoleA[Run] {
    val readLn  = Kleisli(_ => Eval.always(StdIn.readLine()))
    val printLn = Kleisli(msg => Eval.always(println(msg)))
  }

  def foo[->[_, _]: Arr](implicit console: ConsoleA[->]): Unit -> Unit = {
    import console._
    arr[->] { () =>
      val in = readLn()
      printLn(in)
    }
  }
}

trait Klsl[+F[_], A, B] {
  def run(a: A): F[B]
}
object Klsl {
  implicit def arr[F[_]: Monad]: Arr[Klsl[F, *, *]] = new Arr[Klsl[F, *, *]] {
    def lift[A, B](f: A => B): Klsl[F, A, B] = f(_).pure[F]
    def split[A, B, C, D](f: Klsl[F, A, B], g: Klsl[F, C, D]): Klsl[F, (A, C), (B, D)] = {
      case (a, b) => f.run(a).flatMap(c => g.run(b).map((c, _)))
    }
    def compose[A, B, C](f: Klsl[F, B, C], g: Klsl[F, A, B]): Klsl[F, A, C] =
      a => g.run(a).flatMap(f.run)
  }
}

object Console2 {
  type Run[A, B] = Klsl[Eval, A, B]

  val readLn: Run[Unit, String]  = _ => Eval.always(StdIn.readLine())
  val printLn: Run[String, Unit] = msg => Eval.always(println(msg))

  def foo: Run[Unit, Unit] =
    arr[Run] { () =>
      val in = readLn()
      printLn(in)
    }
}
