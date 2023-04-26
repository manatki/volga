package volga.util.collections

import scala.annotation.tailrec
import scala.collection.Factory

extension [A](xs: IterableOnce[A])
    def foldErr[B, E](b: B)(f: (B, A) => Either[E, B]): Either[E, B] =
        val i                                 = xs.iterator
        @tailrec def go(acc: B): Either[E, B] =
            if i.hasNext then
                f(acc, i.next()) match
                    case l: Left[?, ?] => l
                    case Right(b1)     => go(b1)
            else Right(b)
        go(b)

    def foldOpt[B, E](b: B)(f: (B, A) => Option[B]): Option[B] =
        val i                              = xs.iterator
        @tailrec def go(acc: B): Option[B] =
            if i.hasNext then
                f(acc, i.next()) match
                    case None     => None
                    case Some(b1) => go(b1)
            else Some(b)
        go(b)
    end foldOpt
end extension

extension [A, T[x] <: IterableOnce[x]](xs: T[A])
    def mapAccumulate[B, C](b: B)(f: (B, A) => (B, C))(using T: Factory[C, T[C]]): (B, T[C]) =
        val builder = T.newBuilder
        var acc     = b
        xs.foreach { x =>
            val (acc1, c) = f(acc, x)
            acc = acc1
            builder += c
        }
        (acc, builder.result())
    end mapAccumulate

    def mapErr[E, B](f: A => Either[E, B])(using T: Factory[B, T[B]]): Either[E, T[B]] =
        val builder               = T.newBuilder
        val xit                   = xs.iterator
        def go(): Either[E, T[B]] =
            if xit.hasNext then
                f(xit.next()) match
                    case Left(e)  => Left(e)
                    case Right(b) =>
                        builder += b
                        go()
            else Right(builder.result())
        go()
    end mapErr
end extension
