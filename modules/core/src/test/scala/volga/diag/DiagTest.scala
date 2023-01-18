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
import Nat.{OfInt, Zero, Succ}

type Label[A, B]            = String
type TT[N <: Int, M <: Int] = FreeProp[Label, Nat.OfInt[N], Nat.OfInt[M]]

def node(name: String, x: Int, y: Int): TT[x.type, y.type] = FreeProp.Embed(name)

val xxx = {
    val a = node("a", 0, 1)
    val b = node("b", 1, 0)
    a >>> b
}

val yyy = {
    val a = node("a", 0, 2)
    val b = node("b", 1, 2)
    val c = node("c", 2, 1)
    val d = node("d", 2, 0)

    a >>> (FreeProp.idInt[1] >< b) >>> (c >< FreeProp.idInt[1]) >>> d
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
    showGraph(yyy)
end DiagTest
