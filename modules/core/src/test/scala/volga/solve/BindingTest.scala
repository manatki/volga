package volga.solve

import volga.syntax.solve.StageList
import volga.syntax.parsing.{STerm, App}
import volga.syntax.solve.StageList.Out
import volga.syntax.solve.BinHistory as BH
import scala.annotation.threadUnsafe
import volga.syntax.solve.Bin

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
            for (in, out, elem) <- pairs.lazyZip(list)
            do
                assertEquals(
                  Bin.fromElements(in).modAll(elem.adaptation.map(_.op)),
                  Right(Bin.fromElements(out))
                )
            this

        def list = result.right.get.toVector

        def listNoHistory = result.right.get.toVector.map(_.copy(adaptation = Vector()))
    end syntax

    test("identity adaptation"):
        assertEquals(
          syntax("a")()("a").list,
          Vector:
              Out(Vector("a"), Vector("a"), Vector()),
        )

    test("swap"):
        val history1 = Vector:
            BH.HSwap("a", "b")
        assertEquals(
          syntax("a", "b")()("b", "a").list,
          Vector:
              Out(Vector("a", "b"), Vector("b", "a"), Vector(), history1),
        )

    test("complex swap"):
        val history1 = Vector:
            BH.HSwap("a", "b")
        assertEquals(
          syntax("a", "b", "c")()("b", "c", "a")
              .checkAdaptations()
              .listNoHistory,
          Vector:
              Out(Vector("a", "b", "c"), Vector("b", "c", "a"), Vector()),
        )

end BindingTest
