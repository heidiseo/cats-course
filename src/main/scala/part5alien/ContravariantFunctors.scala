package part5alien

import cats.kernel.Monoid

object ContravariantFunctors {

  trait Format[T] { self =>
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = s"$value?"
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object Boolean extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] = f.contramap[Option[T]](_.getOrElse(m.empty))

  def contramap[A, T](func: A => T)(implicit f: Format[T]): Format[A] = new Format[A] {
    override def format(value: A): String = f.format(func(value))
  }

  // Map applies transformations in sequence and Contramap does it in REVERSE sequence

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._ // implicit Show[Int]
  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))


  def main(args: Array[String]): Unit = {

  }

}
