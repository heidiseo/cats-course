package part2abstractmaths

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._

  val numbers = (1  to 100).toList

//  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
//    list.foldLeft()(_ |+| _)

  // MONOIDS - provide empty value
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 44)
  val zero = intMonoid.empty

  import cats.instances.string._
  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("sdf", "ee")


  import cats.instances.option._

  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int])
  val combineOption2 = Monoid[Option[Int]].combine(Option(3), Option(4))

  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
      list.foldLeft(monoid.empty)(_ |+| _)

  val phoneBooks = List(
    Map(
      "Alice" -> 334,
      "bob" -> 444
    ),
    Map(
      "charlie" -> 234,
      "daniel" -> 23423,
    ),
    Map(
      "tina" -> 255
    )
  )

  import cats.instances.map._

  val combinedPhoneBooks = combineFold(phoneBooks)

  case class ShoppingCart(items: List[String], total: Double)
  implicit val combineShoppingCarts: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
    ShoppingCart(List.empty, 0),
    (cart1, cart2) => ShoppingCart(cart1.items ++ cart2.items, cart1.total + cart2.total))

  def checkout(shoppingCart: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCart)


  def main(args: Array[String]): Unit = {
    println(combineFold(numbers))
    println(combineFold(List("33", "%fg")))
    println(combinedPhoneBooks)
    println(checkout(List(ShoppingCart(List("sf", "wer"), 44.44), ShoppingCart(List("sdf", "sdf"), 345.44))))
  }


}
