package part2abstractmaths

import scala.util.Random

object Semigroups {

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intComb = naturalIntSemigroup.combine(1, 2)

  import cats.instances.string
  val naturalStringSemigroup = Semigroup[String]
  val stringComb = Semigroup.combine("I love ", "Cats")

  def reduceInt(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]) = list.reduce(semigroup.combine)


  val numbers = (1 to 10).toList

  import cats.instances.double._
  import cats.instances.long._
  import cats.Eq

  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (exp1, exp2) =>
    Expense(Math.max(exp1.id, exp2.id), exp1.amount + exp2.amount)}

  // extension methods from Semigroup - |+|
  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3 // requires the presence of implicit Semigroup[Int]
  val aCombinedExpense = Expense(3, 44.55) |+| Expense(4, 55.6)

  val exp1 = Expense(1, 33.33)
  val exp2 = Expense(12, 11.11)
  val exp3 = Expense(45, 11.11)

  def reduceThings2[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _)


  def main(args: Array[String]): Unit = {
    println(intComb)
    println(stringComb)
    println(reduceInt(numbers))

    val expenses = List(exp1, exp2, exp3)
    println(reduceThings(expenses))
    println(aCombinedExpense)
    println(reduceThings2(expenses))
  }

}
