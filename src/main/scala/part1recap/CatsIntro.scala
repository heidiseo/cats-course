package part1recap

object CatsIntro {

  // Eq
  val aComparison = 2 == "a string"

  // part 1 - type class import
  import cats.Eq

  // part 2 - import type class instances for the types you need
  import cats.instances.int._

  // part 3 - use the TC API
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(3, 4)
  // val anUnsafeComparison = intEquality.eqv(3, "") - doesn't compile!

  // part 4 - use extension methods (if applicable)
  import cats.syntax.eq._
  val anotherTypeSafeComparison = 2 === 3 //false
  // val anotherUnSafeTypeSafeComparison = 2 === "" - doesn't compile!
  val neqComparison = 2 =!= 3 // true

  // part 5 - extending the TC operations to composite types e.g. lists
  val aListComparison = List(2) === List(3)

  // part 6 - create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance((car1, car2) => car1.price === car2.price)

  val compareTwoToyCars = ToyCar("sdw", 23.33) === ToyCar("sd", 23.33)


  // import all
//  import cats._
//  import cats.implicits._

}
