package part2abstractmaths

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  val numbers = List(1,2,4)
  val charsList = List("a", "b", "c")

  for {
    n <- numbers
    c <- charsList
  } yield (n, c)

  numbers.flatMap(n => charsList.map(c => (n, c)))

  val numberOption = Option(2)
  val charOption = Option('d')

  numberOption.flatMap(n => charOption.map(c => (n, c)))

  for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture = Future(42)
  val charFuture = Future('s')

  numberFuture.flatMap(n => charFuture.map(c => (n, c)))

  for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] =
      flatMap(ma)(x => pure(f(x)))
  }

  import cats.Monad
  import cats.instances.option._

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._

  val listMonad = Monad[List]
  val aList = listMonad.pure(4)
  val aTransformedList = listMonad.flatMap(aList)(l => List(l, l + 1))

  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(5)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(f => Future(f, f + 1))

  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(numbers: Option[Int], chars: Option[Char]): Option[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsFuture(numbers: Future[Int], chars: Future[Char]): Future[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = {
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))
  }

  // extension method
  import cats.syntax.applicative._ // pure is here
  val oneOption = 1.pure[Option]
  val oneList = 1.pure[List]

  import cats.syntax.flatMap._
  val oneOptionTrans = oneOption.flatMap(x => (x + 1).pure[Option])

  // Monads are Functors - monads can call map because of functors
  import cats.syntax.functor._
  val oneOptionMapped = oneOption.map(_ + 3)

  val composedOption = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

   def shortendGetPairs[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = for {
     a <- ma
     b <- mb
   } yield (a, b)

  def main(args: Array[String]): Unit = {
    println(shortendGetPairs(numbers, charsList))
    println(shortendGetPairs(numberOption, charOption))
    shortendGetPairs(numberFuture,  charFuture).foreach(println)
  }
}
