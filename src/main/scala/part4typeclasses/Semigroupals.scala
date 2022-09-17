package part4typeclasses


import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._

  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future(Success("the meaning of life, 42))

  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b")) // List((1, "a"), (1, "b), (2, "a"), (2, "b"))

  import cats.Monad
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
//    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  trait MyMonad[M[_]] extends MySemigroupal[M] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))
    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] =
      flatMap(fa)(a => map(fb)(b => (a, b)))
  }

  //MONADS EXTEND SEMIGROUPALS

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr]
  val invalidsComb = validatedSemigroupal.product(
    Validated.invalid(List("a", "b")),
    Validated.invalid(List("C"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherComb = eitherSemigroupal.product(
    Left(List("a, b")),
    Left(List("c"))
  )

  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }



  def main(args: Array[String]): Unit = {

    println(aTupledOption)
    println(aNoneTupled)
    println(aTupledFuture)
    println(zipListSemigroupal.product(List(1, 2), List("a", "b", "c")))
  }

}
