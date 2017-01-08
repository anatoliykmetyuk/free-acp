package freeacp

import cats._
import cats.instances.list._

trait Tree[S[_]] {
  implicit val S: Suspended[S]
  implicit val F: Functor[S]

  def terminal(t: Tree[S]): Boolean = t.isInstanceOf[Success[S]] || t.isInstanceOf[Failure[S]]

  def rewrite: PartialFunction[Tree[S], Tree[S]] = _ match {
    // Call
    case Call(t) => t()

    // Sequence
    case Sequence(xs) if xs.contains(Loop()) =>
      def seq: Tree[S] = Sequence( xs.filter(_ != Loop()) :+ new Suspend(S.suspend { seq }) { override def toString = "loopContinuation" } )
      seq
    case Sequence(Nil             ) => Success()
    case Sequence(Sequence(a) :: x) => Sequence(a ++ x)
    case Sequence(Call(t)     :: x) => Sequence(t() :: x)
    case Sequence(Success()   :: x) => Sequence(x)
    case Sequence(Failure()   :: x) => Failure()

    // Choice
    case Choice(Nil) => Failure()
    case Choice  (Choice(x) :: y) => Choice(x ++ y)  // choice-associativity
    case Sequence(Choice(x) :: y) => Choice(x.map { t => Sequence(t :: y) }) // seq-associativity
    case Choice(x) if x.contains(Success()) => Success()
    case Choice(x) if x.contains(Failure()) => Choice(x.filter(_ != Failure()))
    case Choice(x) if x.exists(!resume.isDefinedAt(_)) => Choice(x.map {
      case a if !resume.isDefinedAt(a) => rewrite.apply(a)
      case a => a
    })
  }

  def resume: PartialFunction[ Tree[S], List[S[Tree[S]]] ] = _ match {
    // Atom
    case Suspend(r: S[Tree[S]]) => List(r)
    
    // Sequence
    case Sequence(Suspend(r: S[Tree[S]]) :: x) => List( F.map(r) { rs => Sequence(rs :: x) } )

    // Choice
    case Choice(x @ _ :: _) if x.forall(resume.isDefinedAt) => x.flatMap(resume) // Foldable[List].combineAll(x.map(resume))(S.monoidK.algebra[Tree[S]])
  }

  def step: Tree[S] = {
    @annotation.tailrec def rewriteLoop(x: Tree[S]): Tree[S] =
      if (rewrite.isDefinedAt(x)) rewriteLoop(rewrite apply x) else x
    rewriteLoop(this)
  }

  def run(debug: Boolean = false)(implicit C: Comonad[S], SG: ChoiceK[S]): Result[S] =
    runM(new (S ~> S) { override def apply[A](x: S[A]): S[A] = x }, debug)

  // See Cats' Free's `runM`
  def runM[G[_]: Suspended, Functor](f: S ~> G, debug: Boolean = false, steps: Int = 1000)(implicit G: Comonad[G], M: ChoiceK[G]): Result[G] = {
    @annotation.tailrec
    def loop(t: Tree[S], i: Int): Result[G] = if (i > 0) {
      if (debug) println(s"\nDEBUG:\n$t")
      t match {
        case _: Success[S] => Success[G]()
        case _: Failure[S] => Failure[G]()
        case t =>
          loop ({ if (!resume.isDefinedAt(t)) rewrite.apply(t) else Comonad[G].extract { Foldable[List].combineAll(resume.apply(t).map(f.apply[Tree[S]]))(M.algebra[Tree[S]]) } }, i - 1)
      }
    } else throw new StackOverflowError("Too many execution steps!")
    loop(this, steps)
  }

  def *(other: Tree[S]) = Sequence.apply[S](this, other)
  def +(other: Tree[S]) = Choice  .apply[S](this, other)
}

case class Suspend [S[_]](a :     S[Tree[S]])(implicit val S: Suspended[S], val F: Functor[S]) extends Tree[S] { override def toString = s"suspend"                    }
case class Call    [S[_]](t : () => Tree[S] )(implicit val S: Suspended[S], val F: Functor[S]) extends Tree[S] { override def toString = s"call"                       }
case class Sequence[S[_]](ts:  List[Tree[S]])(implicit val S: Suspended[S], val F: Functor[S]) extends Tree[S] { override def toString = s"[*](${ts.mkString(" * ")})" }
case class Choice  [S[_]](ts:  List[Tree[S]])(implicit val S: Suspended[S], val F: Functor[S]) extends Tree[S] { override def toString = s"[+](${ts.mkString(" + ")})" }

trait Result[S[_]] extends Tree[S]
case class Success[S[_]]()(implicit val S: Suspended[S], val F: Functor[S]) extends Result[S] { override def toString = "1" }
case class Failure[S[_]]()(implicit val S: Suspended[S], val F: Functor[S]) extends Result[S] { override def toString = "0" }

case class Loop[S[_]]()(implicit val S: Suspended[S], val F: Functor[S]) extends Tree[S] { override def toString = "..." }

object Sequence {
  def apply[S[_]](ts: Tree[S]*)(implicit S: Suspended[S], F: Functor[S]): Sequence[S] = Sequence[S](ts.toList)
}

object Choice {
  def apply[S[_]](ts: Tree[S]*)(implicit S: Suspended[S], F: Functor[S]): Choice[S] = Choice[S](ts.toList)
}

