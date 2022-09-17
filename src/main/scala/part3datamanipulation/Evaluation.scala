package part3datamanipulation

object Evaluation {

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    println("computing now")
    2342
  }

  val redoEval = Eval.always {
    println("computing again")
    2345
  }

  val delayedEval = Eval.later {
    println("delayed - computing again")
    2325
  }

  val evalEx1: Eval[Int] = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  /*
  now
  "delayed - computing again"
  again
  "computing again"
  a + b + c+ d
   */

  /*
  again
  again
  sum
   */

  val dontRecompute = redoEval.memoize

  def defer[T](eval: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => eval)


  def main(args: Array[String]): Unit = {
    println(evalEx1.value)
    println(evalEx1.value)

  }
}


