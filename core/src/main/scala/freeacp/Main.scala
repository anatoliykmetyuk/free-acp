package freeacp

import cats.Eval

import scala.concurrent.{Future, Promise, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

import LanguageT._

object Main extends App with EvalImpl with FutureImpl with SayElem {
  def a1 = atom { () }
  def a0 = atom { throw new RuntimeException }

  val x = a0
  val y = (Choice[LanguageT](Nil) + Choice[LanguageT](Nil) + Choice[LanguageT](List(Choice[LanguageT](Nil))) + Sequence(List(Choice[LanguageT](Nil))) + a1 + Choice[LanguageT](Nil) + Sequence(List(a0)) + a0 + δ)
  val z = (Sequence[LanguageT](Nil) * Sequence[LanguageT](Nil) * a1 * Sequence[LanguageT](Nil) * Sequence(List(a0)) * Choice(List(ε)))

  val t1 = call(a0 * say("Foo")) + call(a1 * say("Bar"))
  val t2 = call(a1)

  println("T1")
  val r1 = t1.runM(compiler[Future](defaultCompiler, sayCompiler), true)
  println(r1)

  println("T2")
  val r2 = t2.runM(compiler[Future](defaultCompiler, sayCompiler), true)
  println(r2)

}

object EvalTest extends App with SayElem with EvalImpl {
  val a = say("Hello", "a")
  val b = say("World", "b")
  val c = say("!", "c")

  val t1 = a
  val t2 = a * Sequence(b, c)
  val t3 = a * δ * b * c
  val t4 = a * (b * c)
  val t5 = a * b + c * δ
  val t6 = Sequence(Sequence(Sequence(b, c, δ))) + Sequence(Sequence(a, b, c))
  val t7 = Sequence(a)
  val t8 = Choice(Nil)

  t5.runM(compiler[Eval](defaultCompiler, sayCompiler), true)
}

object FutureTest extends App with FutureImpl with SayElem with ControlledElem {  
  val (ta, a) = controlled("a")
  val (tb, b) = controlled("b")

  val c = say("Hello"     , "c")
  val d = say("World"     , "d")
  val e = say("Something" , "e")
  val f = say("Else"      , "f")

  val t1: Language = a * c * d + b * e * f
  
  val task = Future { t1.runM(compiler[Future](defaultCompiler, sayCompiler, controlledCompiler), debug = true) }
  ta(ε)

  Await.result(task, Duration.Inf)
}

object FreeAcp extends App with SayElem with EvalImpl {
  val program = atom { println("Hello") } * atom { println("World" ) } * say("Foo")
  program.runM(compiler[Eval](defaultCompiler, sayCompiler), false)
}
