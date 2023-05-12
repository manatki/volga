package volga
package diag

import volga.free.FreeProp
import volga.free.Nat
import volga.free.PropOb
import volga.syntax.smc

class SyntaxTest extends munit.FunSuite:
    given U: SymmetricCat[Diag, PropOb] = FreeProp.propCat
    val prop                            = smc.syntax[Diag, PropOb, Nat.Plus]
    type T  = Diag[prop.I, smc.Reconstruct[PropOb, smc.Results[PropOb, Nat.Zero]]]
    import smc.V
    type V1 = V[Nat.`1`]

    val aNode = node("a", 0, 1)
    val bNode = node("b", 1, 0)
    val cNode = node("c", 1, 3)

    test("indent"):
        val exp        = prop.of1((x: V1) => x)
        val (out, res) = exp.link(Tuple1("x"))
        assertEquals(res, Vector.empty)
        assertEquals(out, Tuple1("x"))

    test("apply"):
        val exp        = prop.of1((v: V1) => bNode(v))
        val (out, res) = exp.link(Tuple1("x"))
        assertEquals(res, Vector("x" -> "b"))
        assertEquals(out, EmptyTuple)

    test("swap"):
        val exp        = prop.of2((a: V1, b: V1) => (b, a))
        val (out, res) = exp.link[String, Label](("x", "y"))
        assertEquals(res, Vector.empty)
        assertEquals(out, ("y", "x"))

    test("complex 1"):
        val exp = prop.of0:
            val x         = aNode()
            val (u, v, w) = cNode(x)
            bNode(v)
            (w, u)

        val (out, res) = exp.link[String, Label](EmptyTuple)
        assertEquals(res, Vector("a" -> "c", "c" -> "b"))
        assertEquals(out, ("c", "c"))

    test("complex 2"):
        val exp = prop.of1: (a: V1) =>
            bNode(a)
            val x = aNode()
            x

        val (out, res) = exp.link[String, Label](Tuple1("x"))
        assertEquals(res, Vector("x" -> "b"))
        assertEquals(out, Tuple1("a"))

    test("complex 3"):
        val exp2: DAG[2, 1] = prop.of2: (a: V1, b: V1) =>
            bNode(a)
            bNode(b)
            val x = aNode()
            x

        val (out, res) = exp2.link[String, Label](("x", "y"))
        assertEquals(res, Vector("x" -> "b", "y" -> "b"))
        assertEquals(out, Tuple1("a"))

end SyntaxTest
