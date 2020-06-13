package sandbox

// import cats._
import cats.implicits._


object Main extends App {
  println("Hello " |+| "Cats!")

  println("CHAPTER 1: IMPLICITS & TYPECLASSES")
  val chpOneNotes = ImplicitNotes

  println("CHAPTER 2: MONOIDS AND SEMIGROUPS")
  val chpTwoNotes = monoids.MonoidSemigroupNotes

  println("Chapter 3: Functors")
  val chpThreeNotes = functors.Functors
}
