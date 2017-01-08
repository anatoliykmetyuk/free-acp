package freeacp.util

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global  // TODO: does not belong here

case class DeactivatableFuture[A](underlying: Future[A], callbacks: List[() => Unit] = Nil) {
  def onDeactivate(cb: () => Unit): DeactivatableFuture[A] =
    this.copy(callbacks = callbacks :+ cb)

  def deactivate() = callbacks.foreach(_())
}
