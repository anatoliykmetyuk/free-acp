package subscript.example

import scala.swing._
import scala.swing.event._

import cats.~>

import freeacp._
import freeacp.LanguageT._
import scala.concurrent.{Future, Promise}

abstract class SimpleSubscriptApplication extends SimpleSwingApplication with FutureImpl with ButtonElem with CallElem {
  override def startup(args: Array[String]) {
    super.startup(args)
    new Thread{override def run={live;quit}}.start()
  }

  def liveScript: Language
  def live: Unit = liveScript.runM(compiler[Future](defaultCompiler, promiseCompiler, callCompiler), true)
}

trait ButtonElem extends PromiseElem {
  def button(b: Button) = {
    val p = Promise[Result[LanguageT]]()
    lazy val reaction: PartialFunction[Event, Unit] = { case _: ButtonClicked => p.success(ε); b.unsubscribe(reaction) }
    b.subscribe(reaction)

    promise(p)
  }
}
