package part2abstractmaths

import cats.Order

object UsingMonads {

  import cats.Monad
  import cats.instances.list._

  val monadList = Monad[List]
  val aSimpleList = monadList.pure(3)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._

  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45)
  val changedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n) else Left("error"))

  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) {
      Left("not available yet")
    } else Right("Amsterdam")

  val orderId = 324L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter = getOrderStatus(orderId).flatMap(trackLocation)
  val orderLocationFor = for {
    status <- getOrderStatus(orderId)
    location <- trackLocation(status)
  } yield location

  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_]: Monad](service: HttpService[M], payload: String): M[String] = {
    for {
      con <- service.getConnection(config)
      resp <- service.issueRequest(con, payload)
    } yield resp
  }

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        h <- cfg.get("host")
        p <- cfg.get("port")
      } yield Connection(h, p)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length >= 20) None
      else Some("accepted")
  }

  object ErrorOrHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (!cfg.contains("host") || !cfg.contains("port")) Left(new RuntimeException("sdf"))
      else Right(Connection(cfg("host"), cfg("port")))

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length >= 20) Left(new RuntimeException("asa"))
      else Right("accepted")
  }



  def main(args: Array[String]): Unit = {

    val responseOption = OptionHttpService.getConnection(config).flatMap(con => OptionHttpService.issueRequest(con, "lkaldk"))
    println(getResponse(OptionHttpService, "lksjldfkj"))

    val responseErrorOr = for {
      con <- ErrorOrHttpService.getConnection(config)
      resp <- ErrorOrHttpService.issueRequest(con, "lskjfl")
    } yield resp

    println(getResponse(ErrorOrHttpService, "aa"))

  }

}
