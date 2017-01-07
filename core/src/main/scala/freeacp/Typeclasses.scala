package freeacp

import cats.{Functor, Eval, ~>}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Suspended[S[_]] extends ( (() => ?) ~> S ) {
  def suspend[A](a: => A): S[A] = apply { () => a }
}

object Suspended {
  def apply[S[_]](implicit e: Suspended[S]) = e

  implicit val eval = new Suspended[Eval] {
    override def apply[A](x: () => A): Eval[A] = Eval.always(x())
  }

  implicit val future = new Suspended[Future] {
    override def apply[A](x: () => A): Future[A] = Future { x() }
  }
}

trait Controlled[S[_]] {
  def obtain[A]: (A => Unit, S[A])
}

object Controlled {
  def apply[S[_]](implicit e: Controlled[S]) = e
}
