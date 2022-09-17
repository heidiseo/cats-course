package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))

  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]
  val success = monadErrorEither.pure(32)
  val failure = monadErrorEither.raiseError[Int]("something is wrong")
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "badness" => 44
    case _ => 42
  }

  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "badnedd" => monadErrorEither.pure(33)
    case _ => Left("something else")
  }

  // Try and Future
  import cats.instances.try_._
  val exception = new RuntimeException("Really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // Failure(exception)

//  implicit cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(3))
  MonadError[Future, Throwable].raiseError(exception) // Failure(exception)

  // applicatives => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applErrorVal = ApplicativeError[ErrorsOr, List[String]]
  // pure, raiseError, handleError, handleErrorWith


  // extension methods
  import cats.syntax.applicative._
  import cats.syntax.applicativeError._
  val extendedSuccess = 53.pure[ErrorsOr]


  def main(args: Array[String]): Unit = {

  }

}
