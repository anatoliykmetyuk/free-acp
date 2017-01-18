# FreeACP
FreeACP is a process algebra engine, inspired by an idea of the free objects. This is a work in progress, for the progress report see [this article](http://akmetiuk.com/blog/2017/01/11/rewriting-process-algebra-part-1-introduction-to-process-algebra.html).

## Example
The code is a Hello World program in FreeACP, commented line-by-line. You can also execute it by cloning this repository, executing `sbt core/run` and selecting `freeacp.HelloWorld`. See also a more advanced [GUI example](https://github.com/anatoliykmetyuk/free-acp/blob/master/lookupframe/src/main/scala/subscript/example/LookupFrame.scala).

```scala
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
```
