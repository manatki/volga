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

    // test("go through swap"):

end BindingTest
