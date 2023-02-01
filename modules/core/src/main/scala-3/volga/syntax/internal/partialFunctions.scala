package volga.syntax.internal

extension [A, B](pf: PartialFunction[A, B])
    def travector: PartialFunction[Iterable[A], Vector[B]] =
        case xs if xs.forall(pf.isDefinedAt) => xs.iterator.map(pf).toVector

object OfInt:
    def unapply(s: String): Option[Int] = s.toIntOption
    
