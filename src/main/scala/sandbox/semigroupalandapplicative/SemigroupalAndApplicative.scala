package sandbox.semigroupalandapplicative

object SemigroupalAndApplicative {
  // Functors can't model all types of program flow, like form validation.
  // We want to return All Errors, not stop at the first error like with Either.
  // For example, this fails fast:
  import cats.syntax.either._ // for CatchOnly

  def parseInt(str: String): Either[String, Int] =
    Either
      .catchOnly[NumberFormatException](str.toInt)
      .leftMap(_ => s"Couldn't read $str")

  for {
    a <- parseInt("a")
    b <- parseInt("b")
    c <- parseInt("c")
  } yield (a + b + c)

  // Another example is the concurrent evaluation of Futures
  // Monadic comprehensions only allows us to execute long-running
  // independent tasks in sequence.

  // Semigroupal encompasses the notion of composing pairs of contexts
  // Parallel converts types with a Monad instance to a related type w/ a Semigroupal instance
  // Applicative extends Semigroupal and Functor: It provides a way of applying functions
  // to parameters within a context. Applicative is the source of the pure method.

  // SECTION 6.1: Semigroupal
  // `cats.Semigroupal` is a type class that allows us to combine contexts
  // If we have two objs of type F[A] and F[B], a Semigroupal[F] allows us to
  // combine them to form F[(A, B)]. In Cats:
  trait SemigroupalDef[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  // `fa` and `fb` are independent of one another: we can compute them in either order
  // Compare this to flatMap which imposes a strict order on its parameters
  // This gives us more freedom when defining instances of Semigroupal that we get w/ Monads

  // SECTION 6.1.1: Joining Two Contexts
  import cats.Semigroupal
  import cats.instances.option._ // for Semigroupal

  // If both params are `Some`, we get a tuple of values back
  // If either param is None, the entire result if None
  Semigroupal[Option].product(Some(123), Some("abc"))
  Semigroupal[Option].product(None, Some(123))

  // SECTION 6.1.2: Joining Three or More Contexts
  // Methods `tuple2` through `tuple22` generalize `product` for different arities
  Semigroupal.tuple3(Option(1), Option(2), Option(3))
  Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])

  // `map2` through `map22` apply a specified function to the values inside 2 to 22 contexts
  println(Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _))
}
