package part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives {
  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper) {
        case (a, b) => f(a, b)
      }

    }

    def ap[B, T](wf:W[B =>T])(wa:W[B]): W[T]
  }

  trait MyApplicative[W[_]] extends MyApply[W] {

    def pure[A](a: A): W[A]
  }

  import cats.Apply
  import cats.instances.option // implicit Apply[Option]
  val applyOption = Apply[Option]
  val funcApp = applyOption.ap(Some((x: Int) => x + 1))(Some(3))

  import cats.syntax.apply._
  val tupleOptions = (Option(1), Option(2), Option(3))
  val optionOfTuple = tupleOptions.tupled // Some((1,2,3))
  val sumOption = tupleOptions.mapN(_ + _ + _)


  def main(args: Array[String]): Unit = {

  }

}
