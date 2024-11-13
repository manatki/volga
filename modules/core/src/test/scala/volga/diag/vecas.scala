package lol.kek.cheburek

import scala.compiletime.*
import scala.collection.BuildFrom

type Instances[K <: AnyKind, F[_ <: K], T <: Tuple] <: Tuple = T match
    case EmptyTuple => EmptyTuple
    case t *: rt    => F[t] *: Instances[K, F, rt]

object Instances:
    case class Of[K <: AnyKind, F[_ <: K], T <: Tuple](instances: Instances[K, F, T]):
        inline def get[Ix <: K]: F[Ix] = find[Instances[K, F, T], F[Ix]](instances)

    private inline def find[T <: Tuple, X](inline t: T): X =
        inline t match
            case t1: (X *: t)  => (t1: (X *: t)).head
            case t1: (h *: t)  => find[t, X]((t1: h *: t).tail)
            case _: EmptyTuple => error("not found")

end Instances

@main def test() =
    type Identifier[name <: String] = ValueOf[name]

    val xss: Instances.Of[String, Identifier, ("Hello", "World")] = Instances.Of(summonAll)

    println(xss.get["Hello"].value)
    println(xss.get["World"].value)

    type BuildInt[T[_]] = BuildFrom[Nothing, Int, T[Int]]

    println(xss)
end test

// type Identifier[name <: String] = ValueOf[name]

// val xss: Instances.Of[String, Identifier, ("Hello", "World")] = Instances.Of(summonAll)

// // xss.get["Hello"]

// 1 + 2
