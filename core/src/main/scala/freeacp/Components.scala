package freeacp

import cats._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import LanguageT._

trait FutureImpl {
  implicit def comonadFuture: Comonad[Future] = new Comonad[Future] {
    override def extract[A](x: Future[A]): A = Await.result(x, Duration.Inf)
    override def map[A, B](x: Future[A])(f: A => B): Future[B] = x.map(f)
    override def coflatMap[A, B](x: Future[A])(f: Future[A] => B): Future[B] = map(coflatten(x))(f)
  }
}

object FutureImpl extends FutureImpl


trait SayElem {
  case class Say(s: String) extends LanguageT[Result[LanguageT]]
  def say(s: String, name: String = "suspend"): Tree[LanguageT] = new Suspend[LanguageT](Say(s)) { override def toString = name }

  def sayCompiler[F[_]: Suspended]: PartialCompiler[F] = _ => new (LanguageT ~> OptionK[F, ?]) {
    override def apply[A](s: LanguageT[A]): Option[F[A]] = ({
      case Say(s) => implicitly[Suspended[F]].apply { () => println(s); ε.asInstanceOf[A] }
    }: PartialFunction[LanguageT[A], F[A]]).lift.apply(s)
  }
}

trait PromiseElem {
  case class PromiseContainer(p: Promise[Result[LanguageT]]) extends LanguageT[Result[LanguageT]]
  def promise(p: Promise[Result[LanguageT]], name: String = "promise") = new Suspend[LanguageT](PromiseContainer(p)) { override def toString = name }

  def promiseCompiler: PartialCompiler[Future] = _ => new (LanguageT ~> OptionK[Future, ?]) {
    override def apply[A](s: LanguageT[A]): Option[Future[A]] = ({
      case PromiseContainer(p) => p.future
    }: PartialFunction[LanguageT[A], Future[A]]).lift.apply(s)
  }
}

trait CallElem {
  case class CallContainer(t: () => Language) extends LanguageT[Tree[LanguageT]]
  def call(t: => Language, name: String = "call") = new Suspend[LanguageT](CallContainer { () => t }) { override def toString = name }

  def callCompiler[F[_]: Suspended]: PartialCompiler[F] = _ => new (LanguageT ~> OptionK[F, ?]) {
    override def apply[A](s: LanguageT[A]): Option[F[A]] = ({
      case CallContainer(t) => implicitly[Suspended[F]].suspend(t())
    }: PartialFunction[LanguageT[A], F[A]]).lift.apply(s)
  }
}
