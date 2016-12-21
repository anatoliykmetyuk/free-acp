package freeacp

import cats.Eval

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen._


object TreeProps extends Properties("Tree") with TreeGens {
  implicit def arbTree = Arbitrary(tree)

  property("Runnability") = forAll { t: Tree =>
    try Tree.run[Eval](t).isInstanceOf[Result]
    catch { case t: Throwable => t.printStackTrace; false }
  }

  property("Choice with `1` as one of the operands must always evaluate to 1") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
    Tree.run[Eval](Choice(t1, t2, Success, t3)) == Success
  }

  property("0x == 0") = forAll { t: Tree => Tree.run[Eval](Sequence(Failure, t)) == Failure     }
  property("1x == x") = forAll { t: Tree => Tree.run[Eval](Sequence(Success, t)) == Tree.run[Eval](t) }
  property("(x + 1) * y == xy + y") = forAll { (x: Tree, y: Tree) =>
    Tree.run[Eval](Sequence(Choice(x, Success), y)) == Tree.run[Eval](Choice(Sequence(x, y), y))
  }
}

trait TreeGens {
  def tree: Gen[Tree] = lzy(oneOf(leaf, operator))
  
  def result: Gen[Result] = oneOf(Success, Failure)

  def suspend: Gen[Suspend[Eval]] = for {
    name <- alphaLowerChar
    res  <- result
  } yield new Suspend[Eval](Eval.now(res)) { override def toString = s"$name$res" }

  def leaf: Gen[Tree] = oneOf(result, suspend)

  def operator: Gen[Tree] = sized { size => for {
    s <- choose(0, size)
    g = resize(size / (s+1), tree)
    operands <- listOfN(s, g)
    root     <- oneOf(Choice(operands), Sequence(operands))
  } yield root }
}

object TreeGens extends TreeGens
