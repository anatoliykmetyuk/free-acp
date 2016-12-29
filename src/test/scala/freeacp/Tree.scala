package freeacp

import cats.Eval

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Gen._


object TreeProps extends Properties("Tree") with TreeGens {
  import LanguageT._

  implicit def arbTree = Arbitrary(tree)

  property("Runnability") = forAll { t: Language => run(t).isInstanceOf[Result[LanguageT]] }

  property("Choice with `1` as an option is `1`") =
    forAll { (x: Language, y: Language, z: Language) => x + y + ε + z <-> ε }
  
  property("(x + 1) * y == xy + y") =
    forAll { (x: Language, y: Language) => (x + ε) * y <-> x * y + y }

  // From https://en.wikipedia.org/wiki/Algebra_of_Communicating_Processes#Basic_process_algebra
  property("Commutativity of +"         ) = forAll { (x: Language, y: Language)              => x + y <-> y + x               }
  property("Associativity of +"         ) = forAll { (x: Language, y: Language, z: Language) => (x + y) + z <-> x + (y + z)   }
  property("x + x = x"                  ) = forAll {  x: Language                            => x + x <-> x                   }
  property("Distributivity of * over +" ) = forAll { (x: Language, y: Language, z: Language) => (x + y) * z <-> x * z + y * z }
  property("Associativity of *"         ) = forAll { (x: Language, y: Language, z: Language) => (x * y) * z <-> x * (y * z)   }

  property("δ + x = x" ) = forAll { x: Language => δ + x <-> x }
  property("δ * x = δ" ) = forAll { x: Language => δ * x <-> δ }
  property("ε * x == x") = forAll { x: Language => ε * x <-> x }
}

trait TreeGens extends EvalImpl {
  import LanguageT._

  def run(t: Language) =
    try t.runM(compiler[Eval](defaultCompiler))
    catch { case t: Throwable => t.printStackTrace; throw t }

  implicit class Equivalence(x: Language) {
    def <->(y: Language): Boolean = run(x) == run(y)
  }

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
