package freeacp

import cats.{Functor, MonoidK, ~>}

trait LanguageT[+T]

case class Atom(a: () => Unit) extends LanguageT[Result[LanguageT]]

case class MapLanguage[A, B](t1: LanguageT[A], f : A => B) extends LanguageT[B]
case class SuspendedLanguage[A](x: () => A) extends LanguageT[A]

object LanguageT {
  type Language = Tree[LanguageT]

  def atom(a: => Unit           ): Language = Suspend[LanguageT](Atom( () => a ))
  def atom(name: String)(a: Unit): Language = new Suspend[LanguageT](Atom( () => a )) { override def toString = name }
  def call(t: => Language       ): Language = Call   [LanguageT] { () => t }

  def ε = Success[LanguageT]()
  def δ = Failure[LanguageT]()
  def ω = Loop   [LanguageT]()
 
  implicit val functorLanguage: Functor[LanguageT] = new Functor[LanguageT] {
    override def map[A, B](t: LanguageT[A])(f: A => B): LanguageT[B] = MapLanguage(t, f)
  }

  implicit val suspendedLanguage: Suspended[LanguageT] = new Suspended[LanguageT] {
    override def apply[A](x: () => A): LanguageT[A] = SuspendedLanguage(x)
  }

  type Compiler       [F[_]] = LanguageT ~> F
  type OptionK     [F[_], A] = Option[F[A]]                               // Higher-kinded Option
  type PartialCompiler[F[_]] = Compiler[F] => LanguageT ~> OptionK[F, ?]  // Compiles a part of a language to an Option; has a reference to the main Compiler for recursive compilation
  
  def compiler[F[_]](fs: PartialCompiler[F]*): Compiler[F] = new (LanguageT ~> F) { self =>
    override def apply[A](x: LanguageT[A]): F[A] = fs.toIterator.flatMap(_(self)(x)).next  // TODO: unsafe call to `next`
  }

  def defaultCompiler[F[_]](implicit F: Functor[F], S: Suspended[F]): PartialCompiler[F] = mainCompiler => new (LanguageT ~> OptionK[F, ?]) {
    override def apply[A](x: LanguageT[A]): Option[F[A]] = ({
      case Atom(s) =>
        S.suspend {
          try   { s(); Success[LanguageT]().asInstanceOf[A] }  // TODO: Casts
          catch { case t: Throwable => Failure[LanguageT]().asInstanceOf[A] }
        }

      case MapLanguage(t1, f)   => F.map(mainCompiler(t1))(f)
      case SuspendedLanguage(x) => S(x)      
    }: PartialFunction[LanguageT[A], F[A]]).lift.apply(x)
  }
}
