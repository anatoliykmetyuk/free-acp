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

  implicit val futureMonoidK: MonoidK[Future] = new MonoidK[Future] {
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
}

object FutureImpl extends FutureImpl

trait EvalImpl {
  implicit val evalMonoidK: MonoidK[Eval] = new MonoidK[Eval] {
    override def empty[A]: Eval[A] = Eval.always { ??? }
    override def combineK[A](t1: Eval[A], t2: Eval[A]): Eval[A] =
      t1 match { case t1: Eval[Tree[a]] => t2 match { case t2: Eval[Tree[a]] => t2.map(_.step) }}
  }
}

object EvalImpl extends EvalImpl

trait SayElem {
  case class Say(s: String) extends LanguageT[Result[LanguageT]]
  def say(s: String, name: String = "suspend"): Tree[LanguageT] = new Suspend[LanguageT](Say(s)) { override def toString = name }

  def sayCompiler[F[_]: Suspended]: PartialCompiler[F] = _ => new (LanguageT ~> OptionK[F, ?]) {
    override def apply[A](s: LanguageT[A]): Option[F[A]] = ({
      case Say(s) => Suspended[F].suspend { println(s); ε.asInstanceOf[A] }
    }: PartialFunction[LanguageT[A], F[A]]).lift.apply(s)
  }
}

trait ControlledElem {
  case class ControlledContainer() extends LanguageT[Result[LanguageT]] {
    private[freeacp] var callback: Option[Result[LanguageT] => Unit] = None
    def trigger(a: Result[LanguageT]) = callback.foreach(_.apply(a))
  }

  def controlled(name: String = "controlled"): (Result[LanguageT] => Unit, Language) = {
    val (trigger, ctrldLang) = Controlled[LanguageT].obtain[Result[LanguageT]]
    (trigger, new Suspend[LanguageT](ctrldLang) { override def toString = name })
  }

  implicit val controlledLangauge: Controlled[LanguageT] = new Controlled[LanguageT] {
    override def obtain[A]: (A => Unit, LanguageT[A]) = {
      val c = ControlledContainer()
      (a => c.trigger(a.asInstanceOf[Result[LanguageT]]), c.asInstanceOf[LanguageT[A]])  // TODO: Casts
    }
  }

  def controlledCompiler[F[_]: Controlled]: PartialCompiler[F] = _ => new (LanguageT ~> OptionK[F, ?]) {
    override def apply[A](s: LanguageT[A]): Option[F[A]] = ({
      case c: ControlledContainer =>
        val (trigger, element) = Controlled[F].obtain[A]
        c.callback = Some(trigger)
        element.asInstanceOf[F[A]]
    }: PartialFunction[LanguageT[A], F[A]]).lift.apply(s)
  }
}
