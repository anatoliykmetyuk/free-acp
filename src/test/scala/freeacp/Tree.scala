package freeacp

import cats.{Eval, Comonad}

import org.scalacheck.{Properties, Test, Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen._

import scala.concurrent.Future

import LanguageT._
import FutureImpl._

abstract class TreeLaws[S[_]](name: String)(implicit val S: Suspended[S], val C: Comonad[S]) extends Properties(name) with TreeGens[S] {
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

object EvalLaws   extends TreeLaws[Eval  ]("Eval laws")
object FutureLaws extends TreeLaws[Future]("Future laws")

trait TreeGens[S[_]] {
  import LanguageT._

  implicit val S: Suspended[S]
  implicit val C: Comonad[S]

  def run(t: Language) =
    try t.runM(compiler[S](defaultCompiler))
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
