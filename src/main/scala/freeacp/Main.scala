package freeacp

import cats._
import cats.instances.future._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait Common {
  def atom(f: => Unit, name: String = "suspend") = new Suspend[Eval](Eval.always { f; Success }) { override def toString = name }
  def pln(str: String, name: String = "suspend") = atom(println(str), name)

  implicit val futureComonad: Comonad[Future] = new Comonad[Future] {
    def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
    def extract[A](x: Future[A]): A = Await.result(x, Duration.Inf)
    def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }
}

object Main extends App with Common {
  val a = pln("Hello", "a")
  val b = pln("World", "b")
  val c = pln("!", "c")
  val d = Failure

  val t1 = a
  val t2 = Sequence(a, b, c)
  val t3 = Sequence(a, d, b, c)
  val t4 = Sequence(a, Sequence(b, c))
  val t5 = Choice(Sequence(a, b), Sequence(c, d))
  val t6 = Choice(Sequence(Sequence(Sequence(b, c, d))), Sequence(Sequence(a, b, c)))
  val t7 = Sequence(a)
  val t8 = Choice(Nil)

  Tree.run[Eval](Sequence(Suspend[Eval](Eval.now(Failure))), debug = true)
}

object ArbitraryFunctor extends App with Common {
  val pa = Promise[Result]()
  val pb = Promise[Result]()

  val a = Suspend[Future](pa.future)
  val b = Suspend[Future](pb.future)
  val c = Suspend[Future](Future { println("Hello")    ; Success })
  val d = Suspend[Future](Future { println("World")    ; Success })
  val e = Suspend[Future](Future { println("Something"); Success })
  val f = Suspend[Future](Future { println("Else")     ; Success })

  val t1: Tree = a * c * d ++ b //* e * f
  
  val task = Future(Tree.run[Future](t1, Future.firstCompletedOf, debug = true))
  pa.success(Success)

  Await.result(task, Duration.Inf)
}
