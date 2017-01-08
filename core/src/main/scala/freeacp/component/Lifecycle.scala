package freeacp
package component

import cats._
import LanguageT._

trait LifecycleElem {
  case class LifecycleContainer(wrapped: LanguageT[Result[LanguageT]], onActivate: () => Unit, onDeactivate: () => Unit) extends LanguageT[Result[LanguageT]]

  def withLifecycle(wrapped: LanguageT[Result[LanguageT]])(onActivate: => Unit = (), onDeactivate: => Unit = ()) = LifecycleContainer(wrapped, () => onActivate, () => onDeactivate)

  def lifecycleCompiler[F[_]: Lifecycle]: PartialCompiler[F] = mainCompiler => new (LanguageT ~> OptionK[F, ?]) {
    override def apply[A](s: LanguageT[A]): Option[F[A]] = ({
      case LifecycleContainer(wrapped, onActivate, onDeactivate) =>
        val wrappedF: F[Result[LanguageT]] = mainCompiler(wrapped)
        Lifecycle[F].onActivate  (wrappedF, onActivate)
        Lifecycle[F].onDeactivate(wrappedF, onDeactivate)
        wrappedF.asInstanceOf[F[A]]  // TODO: Cast
    }: PartialFunction[LanguageT[A], F[A]]).lift.apply(s)
  }
}
