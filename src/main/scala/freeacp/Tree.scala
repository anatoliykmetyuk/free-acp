package freeacp

import cats._
import cats.syntax.all._

trait Tree

object Tree {
  def terminal(t: Tree): Boolean = t == Success || t == Failure

  def rewrite[S[_]: Functor](choose: List[S[Tree]] => S[Tree]): PartialFunction[Tree, Tree] = _ match {
    // Sequence
    case Sequence(Nil             ) => Success
    case Sequence(Sequence(a) :: x) => Sequence(a ++ x)
    case Sequence(Success     :: x) => Sequence(x)
    case Sequence(Failure     :: x) => Failure

    // Choice
    case Choice(Nil) => Success
    case Choice  (Choice(x) :: y) => Choice(x ++ y)  // choice-associativity
    case Sequence(Choice(x) :: y) => Choice(x.map { t => Sequence(t :: y) }) // seq-associativity
    case Choice(x) if x.contains(Success) => Success
    case Choice(x) if x.contains(Failure) => Choice(x.filter(_ != Failure))
    case Choice(x) if x.exists(!resume[S](choose).isDefinedAt(_)) => Choice(x.map {
      case a if !resume[S](choose).isDefinedAt(a) => rewrite[S](choose).apply(a)
      case a => a
    })
  }

  def resume[S[_]](choose: List[S[Tree]] => S[Tree])(implicit S: Functor[S]): PartialFunction[Tree, S[Tree]] = _ match {
    // Atom
    case Suspend(r: S[Tree]) => r
    
    // Sequence
    case Sequence(Suspend(r: S[Tree]) :: x) => S.map(r) { rs => Sequence(rs :: x) }

    // Choice
    case Choice(x @ _ :: _) if x.forall(resume[S](choose).isDefinedAt) => choose(x.map(resume[S](choose)))
  }

  def run[S[_]](t: Tree, choose: List[S[Tree]] => S[Tree] = (_: List[S[Tree]]).head, debug: Boolean = false)(implicit S: Comonad[S]): Result = {
    @annotation.tailrec
    def loop(t: Tree): Result = {
      if (debug) println(s"\nDEBUG:\n$t")
      t match {
        case r: Result => r
        case t: Tree   => loop(if (!resume[S](choose).isDefinedAt(t)) rewrite[S](choose).apply(t) else S.extract(resume[S](choose).apply(t)))
      }
    }
    loop(t)
  }

  implicit def toOps(t: Tree): TreeOps = new TreeOps(t)
}

case class Suspend[S[_]](a : S[Tree]) extends Tree { override def toString = s"suspend"                 }
case class Sequence(ts: List[Tree]  ) extends Tree { override def toString = s"(${ts.mkString(" * ")})" }
case class Choice  (ts: List[Tree]  ) extends Tree { override def toString = s"(${ts.mkString(" + ")})" }

trait Result extends Tree
case object Success extends Result { override def toString = "1" }
case object Failure extends Result { override def toString = "0" }

object Sequence {
  def apply(ts: Tree*): Sequence = Sequence(ts.toList)
}

object Choice {
  def apply(ts: Tree*): Choice = Choice(ts.toList)
}

class TreeOps(t: Tree) {
  def *(other: Tree) = Sequence(t, other)
  def ++(other: Tree) = Choice(t, other)
}
