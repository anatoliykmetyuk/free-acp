package freeacp.example

import scala.swing._
import scala.swing.event._

import cats.~>

import freeacp._
import freeacp.engine._
import freeacp.component._
import freeacp.LanguageT._

import freeacp.util.DeactivatableFuture

import scala.concurrent.{Future, Promise}

abstract class SimpleFreeACPApplication extends SimpleSwingApplication
                                           with FutureEngine
                                           with DeactivatableFutureEngine
                                           with ButtonElem
                                           with KeyElem
                                           with GuiElem
                                           with LifecycleElem {
  override def startup(args: Array[String]) {
    super.startup(args)
    new Thread{override def run={live;quit}}.start()
  }

  val top: MainFrame

  def liveScript: Language
  def live: Unit = liveScript.runM(compiler[DeactivatableFuture](defaultCompiler, controlledCompiler, lifecycleCompiler), true)
}

trait ButtonElem extends ControlledElem with LifecycleElem {
  def button(b: Button) = call {
    val (trigger, elem) = controlled()
    lazy val reaction: PartialFunction[Event, Unit] = { case _: ButtonClicked => trigger(ε); b.unsubscribe(reaction); b.enabled = false }
    b.subscribe(reaction)
    b.enabled = true
    
    withLifecycle(elem) {
      b.enabled = false
    }
  }
}

trait KeyElem extends ControlledElem { this: SimpleSwingApplication =>
  def key(keyCode: Key.Value) = call {
    val (trigger, elem) = controlled()
    lazy val reaction: PartialFunction[Event, Unit] = { case k: KeyPressed if k.key == keyCode => trigger(ε); top.reactions -= reaction }
    top.reactions += reaction

    elem
  }
}

trait GuiElem extends ControlledElem {
  def gui(task: => Unit) = {
    val (trigger, elem) = controlled()
    Swing.onEDTWait { task; trigger(ε) }
    elem
  }
}
