package freeacp
package component

import cats._
import LanguageT._

trait ControlledElem {
  case class ControlledContainer() extends LanguageT[Result[LanguageT]] {  // TODO: vars, mutability
    private[freeacp] var _callback    : Option[Result[LanguageT] => Unit] = None
    private[freeacp] var triggeredWith: Option[Result[LanguageT]        ] = None
    
    def callback = _callback
    def callback_=(cb: Option[Result[LanguageT] => Unit]): Unit = { _callback = cb; doTrigger() }

    def doTrigger(): Unit = for {
      r  <- triggeredWith
      cb <- callback
    } cb(r)

    def trigger(a: Result[LanguageT]) =
      if  (triggeredWith.isDefined) throw new IllegalStateException("A controlled element can be triggered only once")
      else { triggeredWith = Some(a); doTrigger }
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
