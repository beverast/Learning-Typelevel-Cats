package sandbox

// import cats._
import cats.implicits._

object Main extends App {
  println("Hello " |+| "Cats!")

  println("\nCHAPTER 1: IMPLICITS & TYPECLASSES")
  val chpOneNotes = ImplicitNotes

  println("\nCHAPTER 2: MONOIDS AND SEMIGROUPS")
  val chpTwoNotes = monoids.MonoidSemigroupNotes

  println("\nChapter 3: FUNCTORS")
  val chpThreeNotes = functors.Functors

  println("\nChapter 4: MONADS")
  val chpFourNotes = monads.Monads

  println("\nCHAPTER 5: MONAD TRANSFORMERS")
  val chpFiveNotes = monadtransformers.MonadTransformers

  println("\nCHAPTER 6: SEMIGROUPAL AND APPLICATIVE")
  val chpSixNotes = semigroupalandapplicative.SemigroupalAndApplicative

  println("\nCHAPTER 7: FOLDABLE AND TRAVERSE")
  val chpSevenNotes = foldableandtraverse.FoldableAndTraverse
}
