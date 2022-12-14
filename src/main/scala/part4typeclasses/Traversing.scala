package part4typeclasses

import cats.data.Validated
import cats.{Applicative, Foldable, Functor, Monad}

import java.util.concurrent.{Executor, Executors}
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (acc, hostName) =>
    val bandFuture: Future[Int] = getBandwidth(hostName)
    for {
    accBandwidth <- acc
    band <- bandFuture
    } yield accBandwidth :+ band
  }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
  val allBandwidthsSeq: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  import cats.syntax.applicative._
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  import cats.syntax.apply._ // mapN

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = list.foldLeft(List.empty[B].pure[F]) { (wAcc, curr) =>
    val wElement: F[B] = func(curr)
    (wAcc, wElement).mapN(_ :+ _)
  }

  def listSequence[F[_] : Applicative, A](list: List[F[A]]): F[List[A]] = listTraverse(list)(identity)

  import cats.instances.vector._
  listSequence(List(Vector(1,2), Vector(3, 4))) // Vector[List[Int]] - all the possible 2-tuples
  listSequence(List(Vector(1,2), Vector(3, 4), Vector(5,6)))  // Vector[List[Int]] - all the possible 3-tuples



  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] = {
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))
  }

  filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2, 4, 6)
  filterAsOption(List(1, 2, 3))(_ % 2 == 0) // None


  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list) { n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"$n failed"))
    }

  filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2, 4, 6))
  filterAsValidated(List(1, 2, 3))(_ % 2 == 0) // Invalid(List("1 failed", "3 failed"))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_] : Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_] : Applicative, A](container: L[F[A]]): F[L[A]] = {
      traverse(container)(identity)
    }
    import cats.Id
    def map[A, B](wa: L[A])(f: A => B): L[B] =
      traverse[Id, A, B](wa)(f)
  }



  def main(args: Array[String]): Unit = {

  }

}
