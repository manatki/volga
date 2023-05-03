package volga.solve

import volga.syntax.solve.StageList
import volga.syntax.parsing.{STerm, App}
import volga.syntax.solve.StageList.Out
import volga.syntax.solve.BinHistory as BH

class BindingTest extends munit.FunSuite:
    class mid(val inputs: String*)(val op: String)(val results: String*)
    class syntax(inputs: String*)(mids: mid*)(output: String*):
        def result = StageList
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

    // test("complex swap"):
    //     assertEquals(
    //       syntax("a", "b", "c")()("b", "c", "a").list,
    //       Vector:
    //           Out(Vector("a", "b", "c"), Vector("b", "c", "a"), Vector()),
    //     )

end BindingTest
