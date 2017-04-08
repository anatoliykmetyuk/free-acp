package freeacp
package engine

import cats._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import freeacp.util.DeactivatableFuture

trait DeactivatableFutureEngine {
  implicit def comonadDeactivatableFuture(implicit cf: Comonad[Future]): Comonad[DeactivatableFuture] = new Comonad[DeactivatableFuture] {
    override def extract[A](x: DeactivatableFuture[A]): A = cf.extract(x.underlying)
    override def map[A, B](x: DeactivatableFuture[A])(f: A => B): DeactivatableFuture[B] = x.copy(underlying = cf.map(x.underlying)(f))
    override def coflatMap[A, B](x: DeactivatableFuture[A])(f: DeactivatableFuture[A] => B): DeactivatableFuture[B] = map(coflatten(x))(f)
  }

  implicit def choiceKDeactivatableFuture(implicit cf: ChoiceK[Future]): ChoiceK[DeactivatableFuture] = new ChoiceK[DeactivatableFuture] {
    override def empty[A] = DeactivatableFuture(cf.empty[A])
    override def combineK[A](f1: DeactivatableFuture[A], f2: DeactivatableFuture[A]): DeactivatableFuture[A] = {
      val combined = cf.combineK(f1.underlying, f2.underlying)
      val result   = DeactivatableFuture(combined, f1.callbacks ++ f2.callbacks)
      combined.onComplete { _ => f1.deactivate(); f2.deactivate(); result.deactivate() }  // TODO: Redundancy
      result
    }
  }

  implicit def controlledDeactivatableFuture(implicit cf: Controlled[Future]): Controlled[DeactivatableFuture] = new Controlled[DeactivatableFuture] {
    override def obtain[A]: (A => Unit, DeactivatableFuture[A]) = {
      val (trigger, future) = cf.obtain[A]
      (trigger, DeactivatableFuture(future))
    }
  }

  implicit def suspendedDeactivatableFuture(implicit sf: Suspended[Future]): Suspended[DeactivatableFuture] = new Suspended[DeactivatableFuture] {
    override def apply[A](x: () => A): DeactivatableFuture[A] = DeactivatableFuture(sf(x))
  }

  implicit val lifecycleDeactivatableFuture: Lifecycle[DeactivatableFuture] = new Lifecycle[DeactivatableFuture] {
    def onDeactivate[A](x: DeactivatableFuture[A], f: () => Unit): DeactivatableFuture[A] = x.onDeactivate(f)
  }
}

object DeactivatableFutureEngine extends DeactivatableFutureEngine
