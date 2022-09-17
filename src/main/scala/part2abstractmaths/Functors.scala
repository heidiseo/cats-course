package part2abstractmaths

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1)
  val aModifiedOption = Option(1).map(_ + 1)
  val aModifiedTry = Try(42).map(_ + 1)

  // simplified def
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._ // incl Functor[List]

  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1)

  import cats.instances.option._ // includes Functor[Option]
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(1))(_ + 1)

  import cats.instances.try_
  val anIncrementedTry = Functor[Try].map(Try(53))(_ + 1)

  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalise
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  trait Tree[+T]
  object Tree {
    // "smart" constructors
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }
  }

  // extension method - map
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(30, Tree.branch(30, Tree.leaf(40), Tree.leaf(20)), Tree.leaf(30))
  val incrementedTree = tree.map(_ + 1)

  // extension - do10x
  def extDo10x[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)


  def main(args: Array[String]): Unit = {
    println(do10x(List(1,2,3)))
    println(do10x(Option(1)))
    println(do10x(Try(1)))
    println(do10x(Tree.branch(33, Tree.leaf(10), Tree.leaf(33))))
    println(extDo10x(Tree.branch(33, Tree.leaf(10), Tree.leaf(33))))
    println(incrementedTree)
  }

}
