package freeacp

import cats._
import cats.instances.future._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

trait Common {
  def atom(f: => Unit, name: String = "suspend") = new Suspend[Eval](Eval.always { f; Success[Eval]() }) { override def toString = name }
  def pln(str: String, name: String = "suspend") = atom(println(str), name)

  implicit val futureComonad: Comonad[Future] = new Comonad[Future] {
    def coflatMap[A, B](fa: Future[A])(f: Future[A] => B): Future[B] = Future(f(fa))
    def extract[A](x: Future[A]): A = Await.result(x, Duration.Inf)
    def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }

  implicit val evalChoice: MonoidK[Eval] = new MonoidK[Eval] {
    def combineK[A](a: Eval[A], b: Eval[A]): Eval[A] = b
    def empty[A]: Eval[Nothing] = Eval.always { ??? }
  }

  implicit val futureChoice: MonoidK[Future] = new MonoidK[Future] {
    def combineK[A](a: Future[A], b: Future[A]): Future[A] = Future.firstCompletedOf(List(a, b))
    def empty[A]: Future[A] = Future.never
  }
}

object Main extends App with Common {
  val a = pln("Hello", "a")
  val b = pln("World", "b")
  val c = pln("!", "c")
  val d = Failure[Eval]()

  val t1 = a
  val t2 = Sequence(a, b, c)
  val t3 = Sequence(a, d, b, c)
  val t4 = Sequence(a, Sequence(b, c))
  val t5 = Choice(Sequence(a, b), Sequence(c, d))
  val t6 = Choice(Sequence(Sequence(Sequence(b, c, d))), Sequence(Sequence(a, b, c)))
  val t7 = Sequence(a)
  val t8 = Choice(Nil)

  t5.run(true)
}

object ArbitraryFunctor extends App with Common {
  val pa = Promise[Result[Future]]()
  val pb = Promise[Result[Future]]()

  val a = Suspend[Future](pa.future)
  val b = Suspend[Future](pb.future)
  val c = Suspend[Future](Future { println("Hello")    ; Success[Future]() })
  val d = Suspend[Future](Future { println("World")    ; Success[Future]() })
  val e = Suspend[Future](Future { println("Something"); Success[Future]() })
  val f = Suspend[Future](Future { println("Else")     ; Success[Future]() })

  val t1: Tree[Future] = a * c * d ++ b * e
  
  val task = Future(t1.run(debug = true))
  pb.success(Success[Future]())

  Await.result(task, Duration.Inf)
}

object FreeAcp extends App {
  trait TalkerT[+T]
  case class Say(s: String) extends TalkerT[Result[TalkerT]]
  type Talker = Tree[TalkerT]

  def say(s: String): Talker = Suspend[TalkerT](Say(s))
  def e: Talker = Success[TalkerT]()
  def d: Talker = Failure[TalkerT]()

  val program: Talker = say("Foo") * say("Bar") * d * say("Char") ++ say("Char") * say("Boo")

  case class  CombineTalker[A](t1: TalkerT[A], t2: TalkerT[A]) extends TalkerT[A]
  case class  MapTalker[A, B](t1: TalkerT[A], f: A => B) extends TalkerT[B]
  case object EmptyTalker extends TalkerT[Nothing]

  implicit def monoidKTalker: MonoidK[TalkerT] = new MonoidK[TalkerT] {
    override def combineK[A](t1: TalkerT[A], t2: TalkerT[A]): TalkerT[A] = CombineTalker(t1, t2)
    override def empty[A]: TalkerT[A] = EmptyTalker
  }

  implicit def functorTalker: Functor[TalkerT] = new Functor[TalkerT] {
    override def map[A, B](t: TalkerT[A])(f: A => B): TalkerT[B] = MapTalker(t, f)
  }

  def compiler[A]: TalkerT[A] => Eval[A] = _ match {
    case Say(s: String) => Eval.now { println(s); Success[TalkerT]().asInstanceOf[A] }

    case CombineTalker(t1, t2) => compiler(t2)
    case MapTalker(t1, f) => Functor[Eval].map(compiler(t1))(f)
    case EmptyTalker => Eval.always { Success[TalkerT]().asInstanceOf[A] }
  }

  program.runM(compiler, true)

}
