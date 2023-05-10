package volga
package diag

import volga.free.FreeProp
import volga.free.Nat
import volga.free.PropOb
import volga.syntax.smc

class SyntaxTest extends munit.FunSuite:
    val prop = smc.syntax[Diag, PropOb]
    type T  = Diag[prop.I, smc.Reconstruct[PropOb, smc.Results[PropOb, Nat.Zero]]]
    import smc.V
    type V1 = V[Nat.`1`]
    test("a -> b"):
        val aNode = node("a", 0, 1)
        val bNode = node("b", 1, 0)
        val cNode = node("c", 1, 3)

        val exp0: DAG[0, 2] = prop.of0 {
            val x         = aNode()
            val (u, v, w) = cNode(x)
            bNode(v)
            (w, u)
        }       

        val exp1: DAG[1, 1] = prop.of1 { (a: V1) =>
            bNode(a)
            aNode()
        }


        val exp2: DAG[2, 1] = prop.of2 { (a: V1, b: V1) =>
            bNode(a)
            bNode(b)
            aNode()
        }
        
        
end SyntaxTest
