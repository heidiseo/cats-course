package part4typeclasses

object Applicatives {

  // Applicatives = Functors + the pure method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2) // List(2)

  import cats.instances.option._
  val optionApplicative = Applicative[Option]
  val anOption = optionApplicative.pure(2) // Some(2)

  import cats.syntax.applicative._
  val aSweetList = 2.pure[List] // List(2)
  val aSweetOption = 2.pure[Option] // Some(2)

  // Monads extend Applicatives
  // Applicatives extend Functors

  import cats.data.Validated
  type ErrorOr[T] = Validated[List[String], T]
  val aValidValue = Validated.valid(2) // "pure"
  val aModifiedValidated: ErrorOr[Int] = aValidValue.map(_ + 1)
  val validatedApplicative = Applicative[ErrorOr]

  // Validated is not monad as monad doesn't have pure function

//  def ap[W[_], B, T](wf: W[B => T])(wa: W[B]): W[T] = ??? // already implemented
  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]): W[(A, B)] = {
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)
  }

  // Applicatives have this ap[W[_], B, T](wf:W[B =>T](wa:W[B): W[T]
  // Applicatives can implement product from Semigroupal
  // => Applicative extend Semigroupal


  def main(arg: Array[String]): Unit = {

  }

}
