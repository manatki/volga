package volga
import net.sourceforge.plantuml.SourceStringReader;
import java.io.{OutputStream}
import java.io.ByteArrayOutputStream
import javax.swing.JFrame
import javax.swing.ImageIcon
import javax.swing.JLabel
import javax.swing.WindowConstants
import java.awt.Toolkit

@main def DiagTest() =

  val png = ByteArrayOutputStream();
  val uml = """|@startuml
               |!pragma layout smetana
               | [Lol]
               | [Kek]
               | [Cheb]
               | Lol --> Kek
               | Lol --> Cheb
               | Cheb --> Kek
               |@enduml
               |""".stripMargin

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
end DiagTest