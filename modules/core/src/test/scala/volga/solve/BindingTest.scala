package volga.solve

import volga.syntax.solve.StageList
import volga.syntax.parsing.{STerm, App}
import volga.syntax.solve.StageList.Out

class BindingTest extends munit.FunSuite:
    class mid(val inputs: String*)(val op: String)(val results: String*)
    def syntax(inputs: String*)(mids: mid*)(output: String*) =
        StageList
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

    test("simple adaptation"):
        assertEquals(
          syntax("a")()("a"),
          Right:
              StageList:
                  Vector:
                      Out(Vector("a"), Vector("a"), Vector(), Vector(), None)
        )

end BindingTest
