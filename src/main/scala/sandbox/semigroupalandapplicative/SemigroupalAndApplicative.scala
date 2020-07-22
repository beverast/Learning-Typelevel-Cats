package sandbox.semigroupalandapplicative

import cats.Applicative

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

  // SECTION 6.1.3: Semigroupal Laws
  // There is only one law for Semigroupal: the product method must be associative

  // SECTION 6.2: Apply Syntax
  import cats.instances.option._ // for Semigroupal
  import cats.syntax.apply._ // for tupled and mapN

  // the `tupled` method is implicitly added to the tuple of Options
  // It uses the Semigroupal for Option to zip the values inside the Options
  // creating a single Option of a tuple:
  println((Option(123), Option("abc")).tupled)

  // In addition to tupled, Cats' apply syntax provides a method called mapN
  // that acceps an implicit Functor and a function of the correct arity
  // to combine the values.
  final case class Cat(name: String, born: Int, color: String)

  (
    Option("Garfield"),
    Option(1978),
    Option("Orange and black")
  ).mapN(Cat.apply)

  // SECTION 6.2.1: Functors and Apply Syntax
  import cats.Monoid
  import cats.instances.int._
  import cats.instances.invariant._
  import cats.instances.list._
  import cats.instances.string._
  import cats.syntax.apply._

  final case class Cat2(
      name: String,
      yearOfBirth: Int,
      favoriteFoods: List[String]
  )

  val tupleToCat: (String, Int, List[String]) => Cat2 = Cat2.apply _
  val catToTuple: Cat2 => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

  implicit val catMonoid: Monoid[Cat2] =
    (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]
    ).imapN(tupleToCat)(catToTuple)

  // Our Monoid allows us to create "empty" Cats, and add Cats together
  import cats.syntax.semigroup._

  val garfield = Cat2("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat2("Heathcliff", 1988, List("Junk food"))

  println(garfield |+| heathcliff)

  // SECTION 6.3: Semigroupal Applied to Different Types
  // Combining Lists with Semigroupal produces some potentially unexpected results
  // We might expect this code to zip the lists, but it returns their cartesian product:
  import cats.instances.list._

  println(Semigroupal[List].product(List(1, 2), List(3, 4)))

  // product applied to Either accumulates errors instead of failing fast
  import cats.instances.either._

  type ErrorOr[A] = Either[Vector[String], A]

  Semigroupal[ErrorOr].product(
    Left(Vector("Error 1")),
    Left(Vector("Error 2"))
  )

  // SECTION 6.3.1: Semigroupal Applied to Monads
  import cats.Monad
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def product[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  // SECTION 6.4: Parallel
  // The Parallel type class and its associated syntax allows us to
  // access alternate semantics for certain models.
  val error1: ErrorOr[Int] = Left(Vector("Error 1"))
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))

  // To collect all the errors we simply replace tupled with its "parallel" version
  // import cats.syntax.parallel._ // for parTupled

  type ErrorOrList[A] = Either[List[String], A]
  val errStr1: ErrorOrList[Int] = Left(List("Error 1"))
  val errStr2: ErrorOrList[Int] = Left(List("Error 2"))

  // (errStr1, errStr2).parTupled   // Missing implicit
  val success1: ErrorOr[Int] = Right(1)
  val success2: ErrorOr[Int] = Right(2)
  val addTwo = (x: Int, y: Int) => x + y

  // (success1, success2).parMapN(addTwo)  // Missing implicit

  // Basic definition of Parallel
  import cats.arrow.FunctionK

  trait ParallelBasic[M[_]] {
    type F[_]

    def applicative: Applicative[F]
    def monad: Monad[M]
    // ~> is a type alias for `FunctionK`, it performs conversion from M to F
    // A normal function A => B converts vals of type A to vals of type B
    // M and F are not types, they are type constructors.
    // A `FunctionK M ~> F is a function from value of type M[A] to val of type F[A]
    def parallel: FunctionK[M, F]
  }

  // As the type param `A` is a generic, `FunctionK` cannot inspect any values
  // contained with the type constructor `M`. The conversion must be performed purely
  // in terms of the structure of the type constructors M and F.
  object optionToList extends FunctionK[Option, List] {
    def apply[A](fa: Option[A]): List[A] =
      fa match {
        case None    => List.empty[A]
        case Some(a) => List(a)
      }
  }

  optionToList(Some(1))
  optionToList(None)

  // In summary, `Parallel` allows us to take a type that has a monad instance and convert it
  // to some related type that instead has an applicative (or semigroupal) instance.
  // The related type will have some useful alternate semantics.
  // As seen, the related applicative for Either allows for accumulation of errors
  // instead of fail-fast semantics.

  // SECTION 6.5: Apply and Applicative
  /*
   * Semigroupals provide a subset of the functionality of a related type class called
   *  and "applicative functor" (applicative for short).
   * `Semigroupal` and `Applicative` effectively provide alternative encodings of the same notion
   *  of joining contexts.
   * Cats models applicatives using two type classes: `cats.Apply` and `cats.Applicative`
   * `cats.Apply` extends Semigroupal and Functor and adds the `ap` method that applies a parameter to a func within a context
   * `cats.Applicative` extends Apply and adds the `pure` method introduced in chapter 4.
   * Here's a simplified definition in code:
   */
  import cats.Functor

  trait BasicApply[F[_]] extends Semigroupal[F] with Functor[F] {
    // Applies parameter `fa` to function ff within a context `F[_]`
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
      ap(map(fa)(a => (b: B) => (a, b)))(fb)
  }

  trait BasicApplicative[F[_]] extends BasicApply[F] {
    // Equivalent to the Monad's pure method
    def pure[A](a: A): F[A]
  }

  /*
   * SECTION 6.5.1: The Hierarchy of Sequencing Type Classes
   *
   * Each type class in the hierarchy represents a particular set of sequencing semantics
   *    introduces a set of characteristic methods, and defines the functionality of its
   *    supertypes in terms of them:
   * Every monad is an applicative
   * Every applicative is a semigroupal
   *  and so on...
   * Apply defines product in terms of ap and map
   * Monad defines product, ap, and map in terms of pure and flatMap
   * Let's consider two hypothetical data types:
   *    Foo is a monad. It's instance implements pure and flatMap, and inherits product, map, ap
   *    Bar is an applicative functor. Its Applicative instance implements pure and ap, and inherits product and map
   * What can we say about these two data types without knowing their implementations?
   *    Strictly, we know more about Foo than Bar: Monad is a subtype of Applicative
   *        We can guarantee properties of Foo (like flatMap)
   *    Bar may have a wider range of behaviors than Foo (fewer laws to obey (no flatMap))
   * The tradeoff: power vs constraint.
   *    The more constraints placed on a data type means more guarantees, but can model fewer behaviors
   * Monads are a sweet-spot in this trade-off:
   *    Flexible enough to model a wide range of behaviors
   *    Restrictive enough to provide strong guarantees
   *    But they impose strict sequencing on computations they model (applicative and semigroupal do not)
   *        So We can use the other two to represent classes of parallel/independent computations as needed
   *
   */

  // SECTION 6.6: Summary
  /*
 * Monads and functors are the most widely used sequencing data types covered so far,
 * but semigroupals and applicatives are the most general. They provide a generic mechanism
 * to combine values and apply functions within a context, from which we can fashion monads and other combinators.
 * Semigroupal and Applicative are most commonly used as a means of combining
 * independent values such as the results of validation rules.
 */
}
