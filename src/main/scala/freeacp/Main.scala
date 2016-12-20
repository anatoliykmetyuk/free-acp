package freeacp

import cats._

object Main extends App {
  def atom(f: => Unit, name: String = "suspend") = new Suspend(Eval.always { f; Success }) { override def toString = name }
  def pln(str: String, name: String) = atom(println(str), name)

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

  Tree.run(Sequence(Suspend(Eval.now(Failure))), true)
}
