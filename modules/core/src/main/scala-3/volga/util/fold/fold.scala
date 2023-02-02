package volga.util.fold

import scala.annotation.tailrec

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
