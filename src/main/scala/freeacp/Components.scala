package freeacp

import cats._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


trait EvalImpl {
  implicit val suspendedEval: Suspended[Eval] = new ( (() => ?) ~> Eval ) {
    override def apply[A](x: () => A): Eval[A] = Eval.always(x())
  }

  implicit val monoidKEval: MonoidK[Eval] = new MonoidK[Eval] {
    override def combineK[A](e1: Eval[A], e2: Eval[A]): Eval[A] = e2
    override def empty[A] = Eval.always[Nothing] { throw new NotImplementedError }
  }
}

trait FutureImpl {
  implicit val suspendedFuture: Suspended[Future] = new ( (() => ?) ~> Future ) {
    override def apply[A](x: () => A): Future[A] = Future { x() }
  }

  implicit val monoidKFuture: MonoidK[Future] = new MonoidK[Future] {
    override def combineK[A](e1: Future[A], e2: Future[A]): Future[A] = Future.firstCompletedOf(List(e1, e2))
    override def empty[A] = Future.never
  }

  implicit val comonadFuture: Comonad[Future] = new Comonad[Future] {
    override def extract[A](x: Future[A]): A = Await.result(x, Duration.Inf)
    override def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = map(coflatten(fa))(f)
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }
}

trait SayElem {
  import LanguageT._

  case class Say(s: String) extends LanguageT[Result[LanguageT]]
  def say(s: String, name: String = "suspend"): Tree[LanguageT] = new Suspend[LanguageT](Say(s)) { override def toString = name }

  def sayCompiler[F[_]: Suspended]: PartialCompiler[F] = _ => new (LanguageT ~> OptionK[F, ?]) {
    override def apply[A](s: LanguageT[A]): Option[F[A]] = ({
      case Say(s) => implicitly[Suspended[F]].apply { () => println(s); ε.asInstanceOf[A] }
    }: PartialFunction[LanguageT[A], F[A]]).lift.apply(s)
  }
}

trait PromiseElem {
  import LanguageT._

  case class PromiseContainer(p: Promise[Result[LanguageT]]) extends LanguageT[Result[LanguageT]]
  def promise(p: Promise[Result[LanguageT]], name: String = "promise") = new Suspend[LanguageT](PromiseContainer(p)) { override def toString = name }

  def promiseCompiler: PartialCompiler[Future] = _ => new (LanguageT ~> OptionK[Future, ?]) {
    override def apply[A](s: LanguageT[A]): Option[Future[A]] = ({
      case PromiseContainer(p) => p.future
    }: PartialFunction[LanguageT[A], Future[A]]).lift.apply(s)
  }
}
