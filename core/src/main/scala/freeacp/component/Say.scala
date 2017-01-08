package freeacp
package component

import cats._
import LanguageT._

trait Say {
  case class SayContainer(s: String) extends LanguageT[Result[LanguageT]]
  def say(s: String, name: String = "suspend"): Tree[LanguageT] = new Suspend[LanguageT](SayContainer(s)) { override def toString = name }

  def sayCompiler[F[_]: Suspended]: PartialCompiler[F] = _ => new (LanguageT ~> OptionK[F, ?]) {
    override def apply[A](s: LanguageT[A]): Option[F[A]] = ({
      case SayContainer(s) => Suspended[F].suspend { println(s); ε.asInstanceOf[A] }
    }: PartialFunction[LanguageT[A], F[A]]).lift.apply(s)
  }
}
