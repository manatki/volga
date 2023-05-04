package volga.solve

import volga.syntax.solve.StageList
import volga.syntax.parsing.{STerm, App}
import volga.syntax.solve.StageList.{Basic, Adapt, VarList}
import volga.syntax.solve.BinHistory as BH
import scala.annotation.threadUnsafe
import volga.syntax.solve.Bin
import volga.syntax.parsing.Var
import scala.annotation.tailrec

class BindingTest extends munit.FunSuite:

    class mid(val inputs: String*)(val op: String)(val results: String*)
    def mid1(inputs: String*)(results: String*): mid =
        val is = inputs.mkString("[", ", ", "]")
        val os = results.mkString("[", ", ", "]")
        mid(inputs*)(s"$is -> $os")(results*)

    class syntax(inputs: String*)(mids: mid*)(output: String*):
        def pairs =
            val ins  = inputs +: mids.map(_.results)
            val outs = mids.map(_.inputs) :+ output
            ins.lazyZip(outs)

        @threadUnsafe lazy val result = StageList
            .fromTerms(
              inputs.toVector,
              mids.view
                  .map(mid =>
                      STerm.Assignment(
                        mid.results.toVector,
                        App(mid.op, mid.inputs.toVector)
                      )
                  )
                  .toVector,
              STerm.Result(output.toVector)
            )
            .withAdaptation

        def checkAdaptations(): this.type =
            val it                                         = pairs.lazyZip(list).iterator
            @tailrec def go(remains: Vector[String]): Unit = if it.hasNext then
                val (in, out, elem)   = it.next()
                val inBinEff          = Bin.fromElements(in)
                val remBin            = Bin.fromElements(remains)
                val inBin             = if remains.isEmpty then inBinEff else Bin.Branch(inBinEff, remBin)
                val outBinRes         = inBin.modAll(elem.adaptation.map(_.op))
                val outBin            = outBinRes.fold((throw _), identity)
                val (outBinEff, keep) = outBin match
                    case _ if elem.next.goThrough.isEmpty => (outBin, Vector())
                    case Bin.Branch(eff, keep)            => (eff, keep.elems)
                    case _                                => throw RuntimeException(s"expecting branch but out is $outBin")
                assertEquals(outBinEff.elems, out)
                go(keep)
            else if remains.nonEmpty then throw RuntimeException(s"something remains $remains")
            go(Vector())
            this
        end checkAdaptations

        def list = result.fold((throw _), _.toVector)

        def listNoHistory = result.right.get.toVector.map(_.copy(adaptation = Vector()))

        def checkList(noHistory: Boolean)(adapts: Adapt[String, String, Any]*): this.type =
            assertEquals(if noHistory then listNoHistory else list, adapts.toVector)
            this

        def error = result.left.get

    end syntax

    test("identity adaptation"):
        syntax("a")()("a").checkList(noHistory = false)(
          Adapt(VarList(Vector("a"), Vector()), VarList(Vector("a"), Vector()), Vector())
        )

    test("swap"):
        syntax("a", "b")()("b", "a").checkList(noHistory = false)(
          Adapt(
            VarList(Vector("a", "b"), Vector()),
            VarList(Vector("b", "a"), Vector()),
            adaptation = Vector(BH.HSwap("a", "b"))
          )
        )

    test("complex swap"):
        syntax("a", "b", "c")()("b", "c", "a")
            .checkAdaptations()
            .checkList(noHistory = true)(
              Adapt(VarList(Vector("a", "b", "c"), Vector()), VarList(Vector("b", "c", "a"), Vector()))
            )

    test("unknown"):
        assertEquals(
          syntax("a")()("b").error,
          StageList.Err.UnknownVar("b", None)
        )

    test("unused"):
        assertEquals(
          syntax("a", "b")()("b").error,
          StageList.Err.UnusedVar("a")
        )

    test("go through"):
        syntax("a", "b")(
          mid("b")("use-b")("c")
        )("a", "c")
            .checkAdaptations()
            .checkList(noHistory = true)(
              Adapt(VarList(Vector("a", "b"), Vector()), VarList(Vector("b"), Vector("a")), binding = Some("use-b")),
              Adapt(VarList(Vector("c"), Vector("a")), VarList(Vector("a", "c"), Vector()))
            )

    test("go through swap"):
        syntax("a", "b", "c")(
          mid("c", "a")("[c, a] -> [d, e]")("d", "e")
        )("b", "e", "d")
            .checkAdaptations()
            .checkList(noHistory = true)(
              Adapt(
                VarList(Vector("a", "b", "c"), Vector()),
                VarList(Vector("c", "a"), Vector("b")),
                binding = Some("[c, a] -> [d, e]")
              ),
              Adapt(VarList(Vector("d", "e"), Vector("b")), VarList(Vector("b", "e", "d"), Vector()))
            )

    test("to through 2 levels"):
        syntax("a", "b", "c")(
          mid("c")("[c] -> [d, e]")("d", "e"),
          mid("e", "a")("[e, a] -> [f, g, h]")("f", "g", "h")
        )("b", "g", "f", "d", "h")
            .checkAdaptations()
            .checkList(noHistory = true)(
              Adapt(
                VarList(Vector("a", "b", "c"), Vector()),
                VarList(Vector("c"), Vector("a", "b")),
                binding = Some("[c] -> [d, e]")
              ),
              Adapt(
                VarList(Vector("d", "e"), Vector("a", "b")),
                VarList(Vector("e", "a"), Vector("b", "d")),
                binding = Some("[e, a] -> [f, g, h]")
              ),
              Adapt(
                VarList(Vector("f", "g", "h"), Vector("b", "d")),
                VarList(Vector("b", "g", "f", "d", "h"), Vector())
              )
            )

    test("to through many levels"):
        syntax()(
          mid1()("a", "b", "c"),
          mid1("c")("d", "e"),
          mid1("d")(),
          mid1()("x", "y"),
          mid1("e", "a")("f", "g", "h"),
          mid1("b", "h")(),
          mid1("x", "g")(),
          mid1("f", "y")("z")
        )("z").checkAdaptations()

    test("to through many levelsv unused"):
        assertEquals(
          syntax()(
            mid1()("a", "b", "c"),
            mid1("c")("d", "e"),
            mid1("d")(),
            mid1()("x", "y"),
            mid1("e", "a")("f", "g", "h"),
            mid1("b")(),
            mid1("x", "g")(),
            mid1("f", "y")("z")
          )("z").error,
          StageList.Err.UnusedVar("h")
        )

    test("to through many levels unknown"):
        assertEquals(
            syntax()(
            mid1()("b", "c"),
            mid1("c")("d", "e"),
            mid1("d")(),
            mid1()("x", "y"),
            mid1("e", "a")("f", "g", "h"),
            mid1("b", "h")(),
            mid1("x", "g")(),
            mid1("f", "y")("z")
            )("z").error,
            StageList.Err.UnknownVar("a", Some("[e, a] -> [f, g, h]"))
        )

end BindingTest
