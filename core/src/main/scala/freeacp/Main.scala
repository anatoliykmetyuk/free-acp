package freeacp

import cats.Eval

import scala.concurrent.{Future, Promise, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

import freeacp.engine._
import freeacp.component._
import LanguageT._
import freeacp.util.DeactivatableFuture

object Main extends App with EvalEngine with FutureEngine with Say {
  def t(n: String) = atom(n) { () }

  val a = t("a")
  val b = t("b")
  val c = t("c")

  val t1 = a * b * c

  val r1 = t1.runM(compiler[Future](defaultCompiler, sayCompiler), true)
  println(r1)
}

object EvalTest extends App with Say with EvalEngine {
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
  val t8: Language = Choice(Nil)

  t5.runM(compiler[Eval](defaultCompiler, sayCompiler), true)
}

object FutureTest extends App with FutureEngine with DeactivatableFutureEngine with Say with ControlledElem with LifecycleElem {  
  val (ta, a) = controlled("a")
  val (tb, b) = controlled("b")

  val c = withLifecycle(say("Hello", "c"))((), { println("Me done!") })
  val d = say("World"     , "d")
  val e = say("Something" , "e")
  val f = say("Else"      , "f")

  val t1: Language = a * c * d + b * e * f
  
  val task = Future { t1.runM(compiler[DeactivatableFuture](defaultCompiler, sayCompiler, controlledCompiler, lifecycleCompiler), debug = true) }
  tb(ε)

  Await.result(task, Duration.Inf)
}

object FreeAcp extends App with Say with EvalEngine {
  val program = atom { println("Hello") } * atom { println("World" ) } * say("Foo")
  program.runM(compiler[Eval](defaultCompiler, sayCompiler), false)
}
