package freeacp

import cats.Eval

import scala.concurrent.{Future, Promise, Await}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

import LanguageT._

object Main extends App with EvalImpl {
  val x = Sequence(atom { throw new RuntimeException })
  val y = atom { println("Success!")        }

  val t1 = x + y
  val t2 = y + x

  println("T1")
  t1.runM(compiler[Eval](defaultCompiler), true)

  println("\n\nT2")
  t2.runM(compiler[Eval](defaultCompiler), true)
}

object EvalTest extends App with EvalImpl with SayElem {
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

object FutureTest extends App with FutureImpl with SayElem with PromiseElem {
  val pa = Promise[Result[LanguageT]]()
  val pb = Promise[Result[LanguageT]]
  
  val a = promise(pa          , "a")
  val b = promise(pb          , "b")

  val c = say    ("Hello"     , "c")
  val d = say    ("World"     , "d")
  val e = say    ("Something" , "e")
  val f = say    ("Else"      , "f")

  val t1: Language = a * c * d + b * e * f
  
  val task = Future { t1.runM(compiler[Future](defaultCompiler, sayCompiler, promiseCompiler), debug = true) }
  pb.success(ε)

  Await.result(task, Duration.Inf)
}

object FreeAcp extends App with EvalImpl with SayElem {
  val program = atom { println("Hello") } * atom { println("World" ) } * say("Foo")
  program.runM(compiler[Eval](defaultCompiler, sayCompiler), false)
}
