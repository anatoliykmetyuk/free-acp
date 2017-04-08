package subscript.example

import scala.language.implicitConversions

import scala.swing._
import scala.swing.event._

import freeacp.LanguageT._


object LookupFrame extends LookupFrameApplication

class LookupFrameApplication extends SimpleSubscriptApplication {
  
  val outputTA = new TextArea           { editable = false }
  val button1  = new Button("Button 1") { enabled  = false }
  val button2  = new Button("Button 2") { enabled  = false }

  val top          = new MainFrame {
    title          = "Two buttons"
    location       = new Point    (100,100)
    preferredSize  = new Dimension(500,300)
    contents       = new BorderPanel {
      add(new FlowPanel(button1, button2), BorderPanel.Position.South)
      add(outputTA, BorderPanel.Position.Center) 
    }
  }

  val lifecycle =
    button(button1) * setText("Hello World"   ) +
    button(button2) * setText("Something Else")
  
  def setText(str: String) = call(gui { outputTA.text = str })
  
  val liveScript = lifecycle * sleep

  def sleep = call { atom { Thread.sleep(Int.MaxValue) } }
}
