package part4typeclasses

import cats.{Eval, Monoid}

object Folding {

  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B])((a, curr) => f(a) :: curr)
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((curr, a) => curr.foldRight(f(a))(_ :: _))
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((a, curr) => if (predicate(a)) a :: curr else curr)
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)(monoid.combine)
  }

  import cats.Foldable
  import cats.instances.list._
  val sum = Foldable[List].foldLeft(List(1,2,3), 4)(_ + _) // 10
  import cats.instances.option._
  val sumOption = Foldable[Option].foldLeft(Option(2), 4)(_ + _) // 6

  // foldRight is stack-safe regardless of your container
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1,2,3), Eval.now(0)) { (a, eval) =>
    eval.map(_ + a)
  }

  import cats.instances.string._
  val mappedConcat = Foldable[List].foldMap(List(1,2,3))(_.toString) // "123"

  // nesting
  import cats.instances.vector._
  val intsNested = List(Vector(1,2,3), Vector(4,2,3))
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  // extension methods
  import cats.syntax.foldable._
  val sum3 = List(1,2,3).combineAll // require Foldable[List], Monoid[List]
  val mappedConcat2 = List(1,2,3).foldMap(_.toString) // require Foldable[List], Monoid[Int]

  def main(args: Array[String]): Unit = {

    import ListExercises._
    val numbers = (1 to 10).toList
    println(map(numbers)(_ + 1))
    println(flatMap(numbers)(x => (1 to x).toList))
    println(filter(numbers)(_ % 2 == 0))
    import cats.instances.int._
    println(combineAll(numbers))
  }

}
