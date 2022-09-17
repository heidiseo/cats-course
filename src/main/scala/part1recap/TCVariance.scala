package part1recap

object TCVariance {

  import cats.Eq
  import cats.instances.int._ // Eq[Int] TC instance
  import cats.instances.option._ // construct a Eq[Option[Int]] TC instance
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)
//  val anIndividualComparison = Some(2) === None

  // rule of thumb: "HAS a T" = covariant, "ACTS on T" = contravariant

  // Cats uses INVARIANCE TCs


}
