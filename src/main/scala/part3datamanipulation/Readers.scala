package part3datamanipulation

import cats.Id

object Readers {

  case class Configuration(dbUserName: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched"
    def getLastOrderId(username: String): Long = 12345
  }
  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  val config = Configuration("heidi", "heekyung", "localhost", 3000, 2, "heidi.seo@gmail.com")

  import cats.data.Reader

  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUserName, conf.dbPassword))
  val dbConn: Id[DbConnection] = dbReader.run(config)

  val heidiOrderStatusReader: Reader[Configuration, String] = dbReader.map(_.getOrderStatus(44))
  val heidiOrderStatus: String = heidiOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersOrderFor = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus
    usersOrderFor.run(config)
  }

  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String): String = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  val emailReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))

  def emailUser(username: String, userEmail: String) = {
    val status = getLastOrderStatus(username)
    val email = emailReader.map(_.sendEmail(userEmail, status))
    email.run(config)
  }

  def main(arg: Array[String]): Unit = {
    println(emailUser("heidi", "heidi.seo@gmail.com"))
  }
}
