package freeacp

import cats._
import cats.syntax.all._

trait Tree

object Tree {
  def terminal(t: Tree): Boolean = t == Success || t == Failure

  def rewrite: PartialFunction[Tree, Tree] = _ match {
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
    case Choice(x) if x.exists(!resume.isDefinedAt(_)) => Choice(x.map {
      case a if !resume.isDefinedAt(a) => rewrite(a)
      case a => a
    })
  }

  def choose(e1: List[Eval[Tree]]): Eval[Tree] = e1.head

  def resume: PartialFunction[Tree, Eval[Tree]] = _ match {
    // Atom
    case Suspend(r: Eval[Result]) => r
    
    // Sequence
    case Sequence(Suspend(r: Eval[Result]) :: x) => r.map {
      case Success => Sequence(x)
      case Failure => Failure
    }

    // Choice
    case Choice(x @ _ :: _) if x.forall(resume.isDefinedAt) => choose(x.map(resume))
  }

  def run(t: Tree, debug: Boolean = false): Result = {
    @annotation.tailrec
    def loop(t: Tree): Result = {
      if (debug) println(s"\nDEBUG:\n$t")
      t match {
        case r: Result => r
        case t: Tree   => loop(if (!resume.isDefinedAt(t)) rewrite(t) else resume(t).value)
      }
    }
    loop(t)
  }
}

case class Suspend (a : Eval[Result]) extends Tree { override def toString = s"suspend"                 }
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
