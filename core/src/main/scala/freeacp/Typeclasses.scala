package freeacp

import cats.{Functor, Eval, MonoidK, ~>}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Suspended[S[_]] extends ( (() => ?) ~> S ) {
  def suspend[A](a: => A): S[A] = apply { () => a }
}

object Suspended {
  def apply[S[_]](implicit e: Suspended[S]) = e
}

trait Controlled[S[_]] {
  def obtain[A]: (A => Unit, S[A])
}

object Controlled {
  def apply[S[_]](implicit e: Controlled[S]) = e
}

trait ChoiceK[S[_]] extends MonoidK[S]

object ChoiceK {
  def apply[S[_]](implicit e: ChoiceK[S]) = e
}

trait Lifecycle[S[_]] {
  def onActivate  (x: S[_], f: () => Unit): Unit
  def onDeactivate(x: S[_], f: () => Unit): Unit
}

object Lifecycle {
  def apply[S[_]](implicit e: Lifecycle[S]) = e
}
