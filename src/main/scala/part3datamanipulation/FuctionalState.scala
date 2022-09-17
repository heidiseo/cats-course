package part3datamanipulation


object FuctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] = State(currCount => (currCount + 1, s"counted $currCount"))
  val (eleven, counted10) = countAndSay.run(10).value

  val firstTransformation: State[Int, String] = State(first => (first + 1, s"added 1 to 10, obtained ${first + 1}"))
  val secondTransformation: State[Int, String] = State(second => (second * 5, s"multiplied with 5, obtained ${second * 5}"))
  val combinedTransformation: State[Int, (String, String)] = firstTransformation.flatMap { first =>
    secondTransformation.map(second => (first, second))
  }

  val combinedTransformation2: State[Int, (String, String)] = for {
    f <- firstTransformation
    s <- secondTransformation
  } yield (f, s)

  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State(shoppingCart => (ShoppingCart(item :: shoppingCart.items, shoppingCart.total + price), shoppingCart.total + price))

  val heidisCart = for {
    _ <- addToCart("a", 400)
    _ <- addToCart("b", 200)
    total <- addToCart("c", 100)
  } yield total

  def inspect[A, B](f: A => B): State[A, B] = State(state => (state, f(state)))

  def get[A]: State[A, A] = State(state => (state, state))

  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))

  def modify[A](f: A => A): State[A, Unit] = State(state => (f(state), ()))


  def main(args: Array[String]): Unit = {
    println(combinedTransformation.run(10).value)
    println(combinedTransformation2.run(10).value)
    println(heidisCart.run(ShoppingCart(List.empty, 0)).value)
  }

}
