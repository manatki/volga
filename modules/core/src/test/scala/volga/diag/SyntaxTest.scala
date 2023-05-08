package volga
package diag

import volga.free.FreeProp
import volga.free.Nat
import volga.free.PropOb
import volga.syntax.smc

class SyntaxTest extends munit.FunSuite:
    val prop = smc.syntax[Diag, PropOb]
    type T = Diag[prop.I, smc.Reconstruct[PropOb, smc.Results[PropOb, Nat.Zero]]]
    test("a -> b"):
        val aNode        = node("a", 0, 1)
        val bNode        = node("b", 1, 0)
        val cNode        = node("c", 1, 3)
        val x: DAG[0, 2] = prop.just {
            val x         = aNode()
            val (u, v, w) = cNode(x)
            bNode(v)
            (w, u)
        }
        import smc.V

        val y: DAG[1, 1] = prop { (a: V[Nat.`1`]) =>
            bNode(a)
            aNode()
        }

end SyntaxTest
