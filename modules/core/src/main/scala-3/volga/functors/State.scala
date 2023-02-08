package volga.functors

import scala.util.control.TailCalls.TailRec
import scala.annotation.tailrec

trait StateCont[S, -E1, -A1, +E2, +A2]:
    self =>
    def onError(s: S, err: E1): State[S, E2, A2]
    def onSuccess(s: S, res: A1): State[S, E2, A2]

    def compose[E3, A3](cont: StateCont[S, E2, A2, E3, A3]): StateCont[S, E1, A1, E3, A3] = new:
        def onError(s: S, err: E1)   = State.Bind(self.onError(s, err), cont)
        def onSuccess(s: S, res: A1) = State.Bind(self.onSuccess(s, res), cont)

sealed trait Result[+E, +A]:
    final def toEither: Either[E, A] = this match
        case State.Success(res) => Right(res)
        case State.Error(err)   => Left(err)

enum State[S, +E, +A]:
    case Modify(f: S => (S, A))
    case Success[S, +A](res: A) extends State[S, Nothing, A], Result[Nothing, A]
    case Error[S, +E](err: E)   extends State[S, E, Nothing], Result[E, Nothing]
    case Bind[S, E1, A1, +E2, +A2](
        cur: State[S, E1, A1],
        cont: StateCont[S, E1, A1, E2, A2]
    )                           extends State[S, E2, A2]

    def flatMap[E1, A1](f: A => State[S, E1, A1]): State[S, E | E1, A1] = Bind(
      this,
      new:
          def onError(s: S, err: E)   = Error(err)
          def onSuccess(s: S, res: A) = f(res)
    )

    def map[A1](f: A => A1): State[S, E, A1] = Bind(
      this,
      new:
          def onError(s: S, err: E)   = Error(err)
          def onSuccess(s: S, res: A) = Success(f(res))
    )

    def handleError[E1, A1](f: E => State[S, E1, A1]): State[S, E1, A | A1] = Bind(
      this,
      new:
          def onError(s: S, err: E)   = f(err)
          def onSuccess(s: S, res: A) = Success(res)
    )

    @tailrec final def run(state: S): (S, Result[E, A]) = this match
        case res: (Success[S, A] | Error[S, E]) => (state, res)
        case Modify(f)                          =>
            val (s, a) = f(state)
            (s, Success(a))
        case Bind(cur, cont)                    =>
            cur match
                case Success(res)      => cont.onSuccess(state, res).run(state)
                case Error(err)        => cont.onError(state, err).run(state)
                case Modify(f)         =>
                    val (s, a) = f(state)
                    cont.onSuccess(s, a).run(s)
                case Bind(cur1, cont2) => Bind(cur1, cont2.compose(cont)).run(state)

end State

object State:
    def update[S](f: S => S): State[S, Nothing, Unit] = Modify(s => (f(s), ()))

    given [S, E]: Monad[State[S, E, _]] with
        def pure[A](x: A) = Success(x)
        extension [A](sa: State[S, E, A])
            def flatMap[B](f: A => State[S, E, B]) = sa.flatMap(f)
            override def map[B](f: A => B)         = sa.map(f)
end State

@main def foooo() =
    import Functor.vectorFunctor
    Vector.range(1L, 100000L).traverse(a => State.update[Long](_ + a) >> State.Success(a)).run(0L) match
        case (s, State.Success(bs)) =>
            println(s"state = $s, sum = ${bs.sum}")
            println("Hello")
        case _                      =>
