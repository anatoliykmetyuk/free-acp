package freeacp
package engine

import cats._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


trait FutureEngine {
  implicit def comonadFuture: Comonad[Future] = new Comonad[Future] {
    override def extract[A](x: Future[A]): A = Await.result(x, Duration.Inf)
    override def map[A, B](x: Future[A])(f: A => B): Future[B] = x.map(f)
    override def coflatMap[A, B](x: Future[A])(f: Future[A] => B): Future[B] = map(coflatten(x))(f)
  }

  implicit val futureChoiceK: ChoiceK[Future] = new ChoiceK[Future] {
    override def empty[A] = Future.successful { LanguageT.δ.asInstanceOf[A] }
    override def combineK[A](t1: Future[A], t2: Future[A]): Future[A] = t1 match { case t1: Future[Tree[a]] => t2 match { case t2: Future[Tree[a]] =>
      def futureOrElse(f1: Future[Tree[a]], f2: Future[Tree[a]]): Future[Tree[a]] =
        f1.flatMap { t => t.step match {
          case _: Failure[_] => f2
          case tx => Future.successful(tx)
        }}

      val x1 = futureOrElse(t1, t2)
      val x2 = futureOrElse(t2, t1)

      Future.firstCompletedOf(List(x1, x2))
    }}
  }

  implicit val controlledFuture: Controlled[Future] = new Controlled[Future] {
    override def obtain[A]: (A => Unit, Future[A]) = {
      val p = Promise[A]()
      (p.success, p.future)
    }
  }

  implicit val future = new Suspended[Future] {
    override def apply[A](x: () => A): Future[A] = Future { x() }
  }
}

object FutureEngine extends FutureEngine
