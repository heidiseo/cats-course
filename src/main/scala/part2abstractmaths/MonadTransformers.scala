//package part2abstractmaths
//
//import java.util.concurrent.{Executor, Executors}
//import scala.concurrent.{ExecutionContext, Future}
//
//object MonadTransformers {
//
//  def sumAllOptions(values: List[Option[Int]]): Int = ???
//
//  import cats.data.OptionT
//  import cats.instances.list._
//
//  val listOfNumOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
//  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))
//  val listOfTupleOptions: OptionT[List, (Int, Char)] = for {
//    char <- listOfCharOptions
//    num <- listOfNumOptions
//  } yield (num, char)
//
//  import cats.data.EitherT
//
//  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("Something wrong"), Right(42), Right(43)))
//  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
//  val futureOfEithers: EitherT[Future, String, Int] = EitherT(Future(Right(45)))
//
//  val bandwidths = Map(
//    "server1.rockthejvm.com" -> 50,
//    "server2.rockthejvm.com" -> 300,
//    "server3.rockthejvm.com" -> 170
//  )
//
//  type AsyncResponse[T] = EitherT[Future, String, T]
//
//  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
//    case None => EitherT(Future(Left(s"Server $server unreachable")))
//    case Some(b) => EitherT(Future(Right(b)))
//  }
//
//  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
//    r1 <- getBandwidth(s1)
//    r2 <- getBandwidth(s2)
//  } yield r1 + r2 > 250
//
//  def generateTraffiSpikeReport(s1: String, s2: String): AsyncResponse[String] = canWithstandSurge(s1, s2).transform()
//
//  def main(args: Array[String]): Unit = {
//    println(listOfTupleOptions.value)
//    println(canWithstandSurge("server1.rockthejvm.com", "server2.rockthejvm.com"))
//  }
//
//}
