package freeacp

import cats.Eval

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen._


object TreeProps extends Properties("Tree") with TreeGens with EvalImpl {
  import LanguageT._

  implicit def arbTree = Arbitrary(tree)

  property("Runnability") = forAll { t: Language =>
    try t.runM(compiler[Eval](defaultCompiler)).isInstanceOf[Result[LanguageT]]
    catch { case t: Throwable => t.printStackTrace; false }
  }

  // property("Choice with `1` as one of the operands must always evaluate to 1") = forAll { (t1: Tree, t2: Tree, t3: Tree) =>
  //   Tree.run[Eval](Choice(t1, t2, Success, t3)) == Success
  // }

  // property("0x == 0") = forAll { t: Tree => Tree.run[Eval](Sequence(Failure, t)) == Failure     }
  // property("1x == x") = forAll { t: Tree => Tree.run[Eval](Sequence(Success, t)) == Tree.run[Eval](t) }
  // property("(x + 1) * y == xy + y") = forAll { (x: Tree, y: Tree) =>
  //   Tree.run[Eval](Sequence(Choice(x, Success), y)) == Tree.run[Eval](Choice(Sequence(x, y), y))
  // }
}

trait TreeGens {
  import LanguageT._

  def tree: Gen[Language] = lzy(oneOf(leaf, operator))
  
  def result: Gen[Result[LanguageT]] = oneOf(ε, δ)

  def suspend: Gen[Suspend[LanguageT]] = for {
    name <- alphaLowerChar
    res  <- result
  } yield new Suspend[LanguageT]( Suspended[LanguageT].apply { () => res } ) { override def toString = s"$name$res" }

  def leaf: Gen[Language] = oneOf(result, suspend)

  def operator: Gen[Language] = sized { size => for {
    s <- choose(0, size)
    g = resize(size / (s+1), tree)
    operands <- listOfN(s, g)
    root     <- oneOf(Choice[LanguageT](operands), Sequence[LanguageT](operands))
  } yield root }
}

object TreeGens extends TreeGens
