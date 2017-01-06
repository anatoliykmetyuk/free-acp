package subscript.example

import scala.language.implicitConversions

import scala.swing._
import scala.swing.event._

import freeacp.LanguageT._

// Subscript sample application: a text entry field with a search button, that simulates the invocation of a background search
//
// Note: the main part of this source file has been manually compiled from Subscript code into plain Scala

object LookupFrame extends LookupFrameApplication

class LookupFrameApplication extends SimpleSubscriptApplication {
  
  val outputTA     = new TextArea        {editable      = false}
  val searchButton = new Button("Go")    {enabled       = true}
  val searchLabel  = new Label("Search") {preferredSize = new Dimension(45,26)}
  val searchTF     = new TextField       {preferredSize = new Dimension(100, 26)}
  
  val top          = new MainFrame {
    title          = "LookupFrame - Subscript"
    location       = new Point    (100,100)
    preferredSize  = new Dimension(500,300)
    contents       = new BorderPanel {
      add(new FlowPanel(searchLabel, searchTF, searchButton), BorderPanel.Position.North)
      add(outputTA, BorderPanel.Position.Center) 
    }
  }

  top.listenTo (searchTF.keys)
  val f = top.peer.getRootPane().getParent().asInstanceOf[javax.swing.JFrame]
  f.setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE) // TBD: does not seem to work on MacOS

  def confirmExit: Boolean = Dialog.showConfirmation(null, "Are you sure?", "About to exit")==Dialog.Result.Yes
  def sleep(time: Long) = Thread.sleep(time)
  
  def liveScript     = ω * call(searchSequence) * atom { println("Hello World!") }
  def searchSequence = button(searchButton) + key(Key.Enter)

  // implicit script vkey(??k: Key.Value) = vkey2: top, ??k

  // script..

  //   liveScript        = ... searchSequence

  //   searchSequence    = searchCommand showSearchingText searchInDatabase showSearchResults
  //   searchCommand     = searchButton + Key.Enter

  //   showSearchingText = @gui: let outputTA.text = "Searching: "+searchTF.text
  //   showSearchResults = @gui: let outputTA.text = "Found: "+here.index+" items"
  //   searchInDatabase  = do* sleep(2000) // simulate a time consuming action
}
