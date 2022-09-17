package part3datamanipulation

object Writers {

  import cats.data.Writer

  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)
  val anIncreasedWriter = aWriter.map(_ + 1) // log stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting")
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something interesting", _ + 1)
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  }

  import cats.instances.vector // Semigroup[Vector]

  val writerA = Writer(Vector("log 1", "log 2"), 10)
  val writerB = Writer(Vector("log b1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  import cats.instances.list

  val anEmptyWriter = aWriter.reset // clear the logs, keep the value

  val desiredValue = aWriter.value // only value
  val logs = aWriter.written // only logs
  val (l, v) = aWriter.run // both


  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(n.toString), n))
  }

  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum (${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def naiveSumAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector.empty, 0)
    else {
      for {
        _ <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- naiveSumAndLog(n - 1)
        _ <- Writer(Vector(s"Computed sum (${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n
    }
  }

  def main(arg: Array[String]): Unit = {
    println(countAndLog(10).run)
  }

}
