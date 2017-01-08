package freeacp
package engine

import cats._

trait EvalEngine {
  implicit val evalChoiceK: ChoiceK[Eval] = new ChoiceK[Eval] {
    override def empty[A]: Eval[A] = Eval.always { ??? }
    override def combineK[A](t1: Eval[A], t2: Eval[A]): Eval[A] =
      t1 match { case t1: Eval[Tree[a]] => t2 match { case t2: Eval[Tree[a]] => t2.map(_.step) }}
  }

  implicit val suspendedEval = new Suspended[Eval] {
    override def apply[A](x: () => A): Eval[A] = Eval.always(x())
  }
}

object EvalEngine extends EvalEngine
