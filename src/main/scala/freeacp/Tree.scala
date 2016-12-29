package freeacp

import cats._
import cats.instances.list._

trait Tree[S[_]] {
  def terminal(t: Tree[S]): Boolean = t.isInstanceOf[Success[S]] || t.isInstanceOf[Failure[S]]

  def rewrite(implicit F: Functor[S], S: MonoidK[S], pure: Pure[S]): PartialFunction[Tree[S], Tree[S]] = _ match {
    // Sequence
    case Sequence(xs) if xs.contains(Loop()) =>
      def seq: Tree[S] = Sequence( xs.filter(_ != Loop()) :+ Suspend( pure(() => seq) ) )
      seq
    case Sequence(Nil             ) => Success()
    case Sequence(Sequence(a) :: x) => Sequence(a ++ x)
    case Sequence(Success()   :: x) => Sequence(x)
    case Sequence(Failure()   :: x) => Failure()

    // Choice
    case Choice(Nil) => Success()
    case Choice  (Choice(x) :: y) => Choice(x ++ y)  // choice-associativity
    case Sequence(Choice(x) :: y) => Choice(x.map { t => Sequence(t :: y) }) // seq-associativity
    case Choice(x) if x.contains(Success()) => Success()
    case Choice(x) if x.contains(Failure()) => Choice(x.filter(_ != Failure()))
    case Choice(x) if x.exists(!resume.isDefinedAt(_)) => Choice(x.map {
      case a if !resume.isDefinedAt(a) => rewrite.apply(a)
      case a => a
    })
  }

  def resume(implicit S: MonoidK[S], F: Functor[S]): PartialFunction[Tree[S], S[Tree[S]]] = _ match {
    // Atom
    case Suspend(r: S[Tree[S]]) => r
    
    // Sequence
    case Sequence(Suspend(r: S[Tree[S]]) :: x) => F.map(r) { rs => Sequence(rs :: x) }

    // Choice
    case Choice(x @ _ :: _) if x.forall(resume.isDefinedAt) => Foldable[List].combineAll(x.map(resume))(S.algebra[Tree[S]])
  }

  def run(debug: Boolean = false)(implicit C: Comonad[S], S: MonoidK[S], pure: Pure[S]): Result[S] = {
    @annotation.tailrec
    def loop(t: Tree[S]): Result[S] = {
      if (debug) println(s"\nDEBUG:\n$t")
      t match {
        case r: Result[S] => r
        case t: Tree  [S] => loop(if (rewrite.isDefinedAt(t)) rewrite.apply(t) else C.extract(resume.apply(t)))
      }
    }
    loop(this)
  }

  // See Cats' Free's `runM`
  def runM[G[_]](f: S ~> G, debug: Boolean = false, steps: Int = 20)(implicit C: Comonad[G], S: MonoidK[S], F: Functor[S], pure: Pure[S]): Result[G] = {
    def loop(t: Tree[S], i: Int): Result[G] = if (i > 0) {
      if (debug) println(s"\nDEBUG:\n$t")
      t match {
        case _: Success[S] => Success[G]()
        case _: Failure[S] => Failure[G]()
        case t => loop ({ if (!resume.isDefinedAt(t)) rewrite.apply(t) else C.extract( f(resume apply t) ) }, i - 1)
      }
    } else throw new NotImplementedError
    loop(this, steps)
  }
}

object Tree {
  implicit def toOps[S[_]](t: Tree[S]): TreeOps[S] = new TreeOps[S](t)
}

case class Suspend [S[_]](a : S[Tree[S]]   ) extends Tree[S] { override def toString = s"suspend"                 }
case class Sequence[S[_]](ts: List[Tree[S]]) extends Tree[S] { override def toString = s"(${ts.mkString(" * ")})" }
case class Choice  [S[_]](ts: List[Tree[S]]) extends Tree[S] { override def toString = s"(${ts.mkString(" + ")})" }

trait Result[S[_]] extends Tree[S]
case class Success[S[_]]() extends Result[S] { override def toString = "1" }
case class Failure[S[_]]() extends Result[S] { override def toString = "0" }

case class Loop[S[_]]() extends Tree[S] { override def toString = "..." }

object Sequence {
  def apply[S[_]](ts: Tree[S]*): Sequence[S] = Sequence[S](ts.toList)
}

object Choice {
  def apply[S[_]](ts: Tree[S]*): Choice[S] = Choice[S](ts.toList)
}

class TreeOps[S[_]](t: Tree[S]) {
  def * (other: Tree[S]) = Sequence[S](t, other)
  def ++(other: Tree[S]) = Choice  [S](t, other)
}
