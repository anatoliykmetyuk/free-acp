package freeacp

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import freeacp.engine.FutureEngine._  // Provides capabilities to run expressions under Future
import freeacp.component.Say._        // Provides `say(string)` AA and the default implementation of it that does `println(string)`
import freeacp.LanguageT._            // Enables us to treat the suspended values `S[_]` as free objects, decoupling definitions of AAs from their implementation

object HelloWorld extends App {
  var i = 0

  // Define AAs we will compose our expression of
  val a = say("Hello")        // Does `println("Hello")` on compilation by `LanguageT ~> Future`
  val b = say("World")
  val c = atom { i = i + 1 }  // Executes the (effectful) code fragment between the curly braces on compilation by `LanguageT ~> Future`
  val d = atom { println(s"Value of i: $i") }

  // Define a process algebra expression in terms of the AAs and the operators.
  // `*` is a sequential composition, AAs will execute one after another.
  val expression: Tree[LanguageT] = a * b * c * d
  
  // Execute the expression
  expression
    .runM(                // This `runM` takes a compiler `LanguageT ~> Future` which implements the AAs defined in terms of `LanguageT`.
      compiler[Future](   // We need to compose several compilers: the default one and the one which implements the `say()` component. `compiler()` function does that. 
        defaultCompiler   // The default compiler implements the essential operations expressed by `LangaugeT` reification: `map: (A => B) => (Future[A] => Future[B])` and `suspend: (() => A) => Future[A]`.
      , sayCompiler       // The `say()` component compiler knows what to do when `say()` AA is executed: `println` whatever string `say` contains.
      ), debug = false    // If `true`, will print the tree rewritings on each step of the execution.
    )
}
