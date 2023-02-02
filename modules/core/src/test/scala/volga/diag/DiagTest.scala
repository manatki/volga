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
import Nat.{OfInt, Zero, Succ}

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
        (FreeProp.idInt[2] >< FreeProp.Swap) >>>
        (FreeProp.idInt[1] >< c >< FreeProp.idInt[1]) >>>
        d
}

val zzz = {
    val x1 = node("X1", 0, 2)
    val x2 = node("X2", 0, 2)
    val y1 = node("Y1", 2, 0)
    val y2 = node("Y2", 2, 0)

    (x1 >< x2) >>>
        (FreeProp.idInt[1] >< FreeProp.Swap >< FreeProp.idInt[1]) >>>
        (y1 >< y2)
}

def showGraph(l: FreeProp[Label, Zero, Zero]): Unit =
    val s     = l.link0[String, Label]
    val names = s.flatMap((a, b) => Vector(a, b)).map(name => s"[$name]").mkString("\n")
    val edges = s.map((a, b) => s"$a --> $b").mkString("\n")
    val uml   = List("@startuml", "!pragma layout smetana", names, edges, "@enduml").mkString("\n")

    showDiagram(uml)

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
    showGraph(xxx >< yyy >< zzz)
end DiagTest
