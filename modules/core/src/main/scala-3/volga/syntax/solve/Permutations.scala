package volga.syntax.solve

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable
import volga.free.Nat.Vec

object Permutations:
    def cycles(xs: Vector[Int]): Vector[Vector[Int]] =
        val seen = mutable.BitSet()
        val bldr = Vector.newBuilder[Vector[Int]]
        for start <- xs.indices if !seen(start) do
            val subldr                    = Vector.newBuilder[Int]
            var i                         = start
            @tailrec def go(i: Int): Unit =
                subldr += i
                seen += i
                val j = xs(i)
                if j != start then go(j)

            go(start)
            bldr += subldr.result()
        end for
        bldr.result()
    end cycles

    def cycleSwaps(xs: Vector[Int]): Vector[(Int, Int)] = xs match
        case head +: rest => rest map head.->
        case _            => Vector()

    def swaps(xs: Vector[Int]): Vector[(Int, Int)] = cycles(xs).flatMap(cycleSwaps)

    def buildPerm[A](xs: Vector[A], ys: Vector[A]): Either[Vector[String], Vector[Int]] =
        val xset = xs.toSet
        val yset = ys.toSet

        val errors = Vector(
          (xset.size != xs.size, () => s"xs has non unique elements $xs"),
          (yset.size != ys.size, () => "ys has non unique elements"),
          (xs.size != ys.size, () => s"xs and ys has different sizes: ${xs.size}, ${ys.size}"),
          (xset != yset, () => s"xs and ys contain different elements : ${xset -- yset} vs ${yset -- xset}")
        ).collect { case (true, f) => f() }
        if errors.isEmpty then Right(xs.map(ys.iterator.zipWithIndex.toMap)) else Left(errors)
    end buildPerm

end Permutations
