package freeacp
package component

import cats._
import LanguageT._

trait LifecycleElem {
  case class LifecycleContainer(wrapped: LanguageT[Result[LanguageT]], onDeactivate: () => Unit) extends LanguageT[Result[LanguageT]]

  def withLifecycle(wrapped: LanguageT[Result[LanguageT]])(onDeactivate: => Unit): Language = new Suspend[LanguageT](LifecycleContainer(wrapped, () => onDeactivate)) { override def toString = s"lifecycle($wrapped)" }
  def withLifecycle(wrapped: Language)(onDeactivate: => Unit): Language = wrapped match { case wrapped: Suspend[LanguageT] =>  // TODO: Cast
    withLifecycle(wrapped.a.asInstanceOf[LanguageT[Result[LanguageT]]])(onDeactivate)  // TODO: Cast
  }

  def lifecycleCompiler[F[_]: Lifecycle]: PartialCompiler[F] = mainCompiler => new (LanguageT ~> OptionK[F, ?]) {
    override def apply[A](s: LanguageT[A]): Option[F[A]] = ({
      case LifecycleContainer(wrapped, onDeactivate) =>
        val wrappedF: F[Result[LanguageT]] = mainCompiler(wrapped)
        Lifecycle[F].onDeactivate(wrappedF, onDeactivate).asInstanceOf[F[A]]  // TODO: Cast
    }: PartialFunction[LanguageT[A], F[A]]).lift.apply(s)
  }
}
