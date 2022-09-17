package part5alien

import cats.Monoid

object InvariantFunctors {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit cryto: Crypto[A]): String = cryto.encrypt(value)
  def decrypt[A](repr: String)(implicit cryto: Crypto[A]): A = cryto.decrypt(repr)

  implicit val caesarCrypto: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)

    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  implicit val doubleCrypto: Crypto[Double] = caesarCrypto.imap(_.toString, _.toDouble)

  implicit val optionCrypto: Crypto[Option[String]] = caesarCrypto.imap(_.getOrElse(""), Option(_))

  implicit def optionTCrypto[T](implicit crypto: Crypto[T], moniod: Monoid[T]): Crypto[Option[T]] = crypto.imap(_.getOrElse(moniod.empty), Option(_))

  import cats.Invariant
  import cats.Show
  import cats.instances.string._
  val showString = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._
  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse(""))

  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[A])(back: B => A): W[B]
    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] =
      contramap(wa)(back)

  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { // covariant functorcont
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = map(wa)(forth)
  }

  def main(args: Array[String]): Unit = {

    val encrypted = encrypt("Let's improve")
    val decrypted = decrypt[String](encrypted)

    println(encrypted)
    println(decrypted)

  }

}
