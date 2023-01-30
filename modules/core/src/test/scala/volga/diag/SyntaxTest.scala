package volga
package diag

import volga.free.FreeProp
import volga.free.Nat
import volga.free.PropOb
import volga.syntax.smc

class SyntaxTest extends munit.FunSuite:
    val prop = syntax.smc.syntax[Diag, PropOb]
    test("a -> b") {
        val aNode        = node("a", 0, 1)
        val bNode        = node("b", 1, 0)
        val x: DAG[0, 0] = prop.just { 
            val x = aNode()
            bNode(x)
        }
    }
end SyntaxTest
