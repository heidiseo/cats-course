package part3datamanipulation

import part3datamanipulation.DataValidation.FormValidation.{FormValidation, checkBlankName, checkEmailFormat, checkPassword, getValue}

import scala.util.Try

object DataValidation {

  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.Valid(4)
  val aInvalidValue: Validated[String, Int] = Validated.invalid("wrong")
  val aTest: Validated[String, Int] = Validated.cond(35 > 25, 44, "wrong")

  def testPrime(n: Int): Boolean = {
    def tailRecPrime(d: Int): Boolean =
      if (d <= 1) true
      else n % d != 0 && tailRecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailRecPrime(Math.abs(n / 2))
  }

  def testNumber(n: Int): Either[List[String], Int] = {
    val isNotEven: List[String] = if (n % 2 == 0) List.empty else List("not even")
    val isNegative: List[String] = if (n < 0) List.empty else List("not positive")
    val isToBig: List[String] = if (n <= 100) List.empty else List("too big")
    val isNotPrime: List[String] = if (testPrime(n)) List.empty else List("not prime")

    if (n % 2 == 0 && n >= 0 && n <= 100 && testPrime(n)) Right(n)
    else Left(isNotEven ++ isNegative ++ isToBig ++ isNotPrime)
  }

  def validateNumber(n: Int): Validated[List[String], Int] = {
    Validated.cond(n % 2 == 0, n,  List("not even"))
      .combine(Validated.cond(n >= 0, n, List("not positive")))
      .combine(Validated.cond(n <= 100, n, List("too big")))
      .combine(Validated.cond(testPrime(n), n, List("not prime")))
  }

  aValidValue.andThen(_ => aInvalidValue)
  aValidValue.ensure(List("fail"))(_ % 2 == 0)
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)
  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(24))
  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("nothing"))
  val tryToValidated:Validated[Throwable, Int] = Validated.fromTry(Try("something".toInt))

  object FormValidation {
    type FormValidation[T] = Validated[List[String], T]

    def getValue(form: Map[String, String], fieldName: String): FormValidation[String] =
      Validated.fromOption(form.get(fieldName), List(s"field - $fieldName must be specified"))

    def checkBlankName(value: String, fieldName: String): FormValidation[String] =
      Validated.cond(value.nonEmpty, value, List(s"field - $fieldName must not be blank"))

    def checkEmailFormat(email: String): FormValidation[String] =
      Validated.cond(email.contains("@"), email, List("email doesn't have @"))

    def checkPassword(password: String): FormValidation[String] =
      Validated.cond(password.length >= 10, password, List("password has less than 10 words"))
  }

  def validateForm(form: Map[String, String]): FormValidation[String] = {
    getValue(form, "Name").andThen(name => checkBlankName(name, "Name"))
      .combine(getValue(form, "Email").andThen(checkEmailFormat))
      .combine(getValue(form, "Password").andThen(checkPassword))
      .map(_ => "success")
  }

  import cats.syntax.validated._
  val aValidMeaningOfLife: Validated[List[String], Int] = 35.valid[List[String]]
  val anError: Validated[String, Int] = "invalid".invalid[Int]


  def main(arg: Array[String]): Unit = {
    println(validateNumber(-15))

    val form = Map(
      "Name" -> "Heidi",
      "Email" -> "heidigmail.com",
      "Password" -> "99"
    )

    println(validateForm(form))
  }

}
