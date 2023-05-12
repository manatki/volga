package volga
package diag
import net.sourceforge.plantuml.SourceStringReader;
import java.io.{OutputStream}
import java.io.ByteArrayOutputStream
import javax.swing.JFrame
import javax.swing.ImageIcon
import javax.swing.JLabel
import javax.swing.WindowConstants
import java.awt.Toolkit
import volga.free.Nat
import volga.syntax.smc
import Nat.{OfInt, Zero, Succ}
import volga.free.PropOb

import volga.free.Nat
import scala.language.unsafeNulls

import volga.free.FreeProp
type Label[A, B]             = String
type Diag[A, B]              = FreeProp[Label, A, B]
type DAG[N <: Int, M <: Int] = FreeProp[Label, Nat.OfInt[N], Nat.OfInt[M]]

def node(name: String, x: Int, y: Int): DAG[x.type, y.type] = FreeProp.Embed(name)

val xxx = {
    val i = node("I", 0, 1)
    val o = node("O", 1, 0)
    i >>> o
}

val yyy = {
    val a = node("a", 0, 3)
    val b = node("b", 1, 2)
    val c = node("c", 2, 1)
    val d = node("d", 3, 0)

    a >>>
        (FreeProp.idInt[1] >< b >< FreeProp.idInt[1]) >>>
        (FreeProp.idInt[2] >< FreeProp.swap2) >>>
        (FreeProp.idInt[1] >< c >< FreeProp.idInt[1]) >>>
        d
}

val zzz = {
    val x1 = node("X1", 0, 2)
    val x2 = node("X2", 0, 2)
    val y1 = node("Y1", 2, 0)
    val y2 = node("Y2", 2, 0)

    (x1 >< x2) >>>
        (FreeProp.idInt[1] >< FreeProp.swap2 >< FreeProp.idInt[1]) >>>
        (y1 >< y2)
}

def showGraph[I: Nat, J: Nat](l: FreeProp[Label, I, J]): Unit =
    val ins        = Nat.Vec.tabulate[I, String](i => s"__in__$i")
    val (outs, s)  = l.link[String, Label](ins)
    val edges      = s.view.collect {
        case (s"__in__$i", out) => s"in$i --> [$out]"
        case (in, out)          => s"[$in] --> [$out]"
    }
    val outPuts    = outs.toVector.zipWithIndex.view.collect {
        case (s"__in__$i", j) => s"in$i --> out$j"
        case (in, i)          => s"[$in] --> out$i"
    }
    val edgeString = (edges ++ outPuts).mkString("\n")
    val uml        = List("@startuml", "!pragma layout smetana", "[TEST]", edgeString, "@enduml").mkString("\n")

    showDiagram(uml)
end showGraph

def showDiagram(uml: String): Unit =
    val png = ByteArrayOutputStream();

    val reader = SourceStringReader(uml);
    // Write the first image to "png"
    val desc   = reader.outputImage(png).getDescription()
    png.close()

    val frame = JFrame()
    frame.add(JLabel(ImageIcon(png.toByteArray(), desc)))
    frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
    frame.pack()
    val dim   = Toolkit.getDefaultToolkit.getScreenSize
    frame.setLocation(dim.width / 2 - frame.getSize().width / 2, dim.height / 2 - frame.getSize().height / 2)
    frame.setVisible(true)
end showDiagram

@main def DiagTest(): Unit =
    showGraph(xxx >< yyy >< zzz >< node("www", 2, 2))
end DiagTest

@main def DiagTest1(): Unit =
    val aNode = node("a", 0, 1)
    val bNode = node("b", 1, 0)
    val cNode = node("c", 1, 3)

    type V1 = smc.V[Nat.`1`]

    given U: SymmetricCat[Diag, PropOb] = FreeProp.propCat[Label]

    val prop = smc.syntax[Diag, PropOb, Nat.Plus]

    val expIdent: DAG[1, 1] = prop.of1((x: V1) => x)
    val expApply: DAG[1, 0] = prop.of1((v: V1) => bNode(v))
    val expSwap: DAG[2, 2]  = prop.of2((a: V1, b: V1) => (b, a))
    val expSwap2: DAG[3, 3] = prop.of3((a: V1, b: V1, c: V1) => (b, a, c))
    val expSwap3: DAG[3, 3] = prop.of3((a: V1, b: V1, c: V1) => (c, a, b))
    val expSwap4: DAG[5, 5] =
        prop.of5((a: V1, b: V1, c: V1, d: V1, e: V1) => (c, a, e, d, b))

    showGraph(expSwap4)
end DiagTest1
