package freeacp

import cats.{Functor, MonoidK, ~>}

trait LanguageT[+T]

case class Atom(a: () => Unit) extends LanguageT[Result[LanguageT]]

case class  CombineLanguage[A   ](t1: LanguageT[A], t2: LanguageT[A]) extends LanguageT[A]
case class  MapLanguage    [A, B](t1: LanguageT[A], f : A => B      ) extends LanguageT[B]
case object EmptyLanguage                                             extends LanguageT[Nothing]
case class  SuspendedLanguage[A](x: () => A) extends LanguageT[A]

object LanguageT {
  type Language = Tree[LanguageT]

  def atom(a: => Unit): Language = Suspend[LanguageT](Atom( () => a ))

  def ε = Success[LanguageT]()
  def δ = Failure[LanguageT]()
  def ω = Loop   [LanguageT]()


  implicit def monoidKLanguage: MonoidK[LanguageT] = new MonoidK[LanguageT] {
    override def combineK[A](t1: LanguageT[A], t2: LanguageT[A]): LanguageT[A] = CombineLanguage(t1, t2)
    override def empty[A]: LanguageT[A] = EmptyLanguage
  }

  implicit def functorLanguage: Functor[LanguageT] = new Functor[LanguageT] {
    override def map[A, B](t: LanguageT[A])(f: A => B): LanguageT[B] = MapLanguage(t, f)
  }

  implicit def suspendedLanguage: Suspended[LanguageT] = new ( (() => ?) ~> LanguageT ) {
    override def apply[A](x: () => A): LanguageT[A] = SuspendedLanguage(x)
  }

  type Compiler       [F[_]] = LanguageT ~> F
  type OptionK     [F[_], A] = Option[F[A]]                            // Higher-kinded Option
  type PartialCompiler[F[_]] = Compiler[F] => LanguageT ~> OptionK[F, ?]  // Compiles a part of a language to an Option; has a reference to the main Compiler for recursive compilation
  
  def compiler[F[_]](fs: PartialCompiler[F]*): Compiler[F] = new (LanguageT ~> F) { self =>
    override def apply[A](x: LanguageT[A]): F[A] = fs.toIterator.flatMap(_(self)(x)).next  // TODO: unsafe call to `next`
  }

  def defaultCompiler[F[_]: Suspended: MonoidK: Functor]: PartialCompiler[F] = mainCompiler => new (LanguageT ~> OptionK[F, ?]) {
    override def apply[A](x: LanguageT[A]): Option[F[A]] = ({
      case Atom(s) =>
        implicitly[Suspended[F]].apply { () =>
          try   { s(); Success[LanguageT]().asInstanceOf[A] }  // TODO: Casts
          catch { case t: Throwable => Failure[LanguageT]().asInstanceOf[A] }
        }

      case CombineLanguage(EmptyLanguage, t2) => mainCompiler(t2)
      case CombineLanguage(t1, EmptyLanguage) => mainCompiler(t1)
      case CombineLanguage(t1, t2           ) => MonoidK [F].combineK(mainCompiler(t1), mainCompiler(t2))

      case MapLanguage(t1, f)   => Functor[F].map(mainCompiler(t1))(f)

      case EmptyLanguage        => implicitly[Suspended[F]].apply { () => Success[LanguageT]().asInstanceOf[A] }
      case SuspendedLanguage(x) => implicitly[Suspended[F]].apply (x)      
    }: PartialFunction[LanguageT[A], F[A]]).lift.apply(x)
  }
}
