package sandbox.monads

object Monads {
  // SECTION 4.1: WHAT IS A MONAD?
  /*
   * Informally, a monadd is anything with a constructor and a flatMap method.
   * All the functors from chp. 3 (Option, List, Future) are also monads.
   * Scala has special syntax to support monads: for comprehensions.
   *   Scala lacks a concrete type to encompase "things that can be flatMapped" (a benefit of Cats)
   *
   * Simply put, A Monad Is A Mechanism For Sequencing Computations.
   * Sounds like a functor, right? Functors are limited:
   *   Functors only allow computations to occur at the beginning of the sequence.
   *   They don't account for further computations at each step in the sequence.
   *       This is where monads come in...
   *
   * A monad's flatMap method allows us to specify what happens next,
   * taking into account an intermediate complication, a sub-computation.
   * Option's flatMap method takes intermediate Options into account.
   *   List's flatMap handles intermediate Lists, and so on.
   *
   * Option allows us to sequence computations that may or may not return values.
   * Here are some examples:
   */

  // Both of these methods may "fail" by returning None
  // flatMap allows us to ignore handling this when we sequence operations
  def parseInt(str: String): Option[Int] =
    scala.util.Try(str.toInt).toOption

  def divide(a: Int, b: Int): Option[Int] =
    if (b == 0) None else Some(a / b)

  /*
   * The first call to parseInt returns a None or Some
   *  If it returns Some, flatMap calls our function and passes us the integer aNum
   * The second call to parseInt returns a None or a Some
   *  If it returms a Some, flatMap calls our function and passes us bNum
   * The call to divide returns a None or a Some, which is our result.
   *  The gives us fail-fast error handling, where a None at any step reults in None overall
   */
  def stringDivideBy(aStr: String, bStr: String): Option[Int] =
    parseInt(aStr).flatMap { aNum =>
      parseInt(bStr).flatMap { bNum => divide(aNum, bNum) }
    }

  // Every monad is a functor, so we can rely on both flatMap and map
  // to sequence operations that do and don't introduce new monads.
  //  We can use for comprehensions to clarify the sequencing behavior too.
  def stringDivideByComprehension(aStr: String, bStr: String): Option[Int] =
    for {
      aNum <- parseInt(aStr)
      bNum <- parseInt(bStr)
      ans <- divide(aNum, bNum)
    } yield ans

  // Future is a monad that sequences computations w/o worrying that they are async:
  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  def doSomethingLongRunning: Future[Int] = Future(2)
  def doSomethingElseLongRunning: Future[Int] = Future(3)

  def doSomethingVeryLongRunning: Future[Int] =
    for {
      result1 <- doSomethingLongRunning
      result2 <- doSomethingElseLongRunning
    } yield result1 + result2

  // SECTION 4.1.1: DEFINITION OF A MONAD
  /*
   * Monadic behavior is formally captured in two operations:
   * `pure` of type `A => F[A]`
   * `flatMap` of type `(F[A] => F[B]) => F[B]`
   *
   * `pure` abstracts over constructors, providing a way to create a new monadic context from a plain value
   * `flatMap` provides the sequencing, extracting the value from a context and generating the next context
   *
   * Below is a simplified version of the Monad type class in Cats:
   */
  trait MonadExample[F[_]] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
  }

  // EXERCISE 4.1.2: Getting Func-y
  // Try defining `map`:
  trait MonadExercise[F[_]] {
    def pure[A](value: A): F[A]
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(a => pure(func(a)))
  }

  // SECTION 4.2: MONADS IN CATS
  // The Monad Type Class
  // `cats.Monad` extends two other type classes:
  //    FlatMap, which provides the flatMap method
  //    Applicative, which provides pure, and also extends Functor to provide map
  // `Monad` provides many methods, including all of Functor's
  import cats.Monad
  import cats.instances.option._
  import cats.instances.list._
  import cats.instances.vector._

  val opt1 = Monad[Option].pure(3)
  val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
  val opt3 = Monad[Option].map(opt2)(a => 100 * a)
  val list1 = Monad[List].pure(3)
  val list2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
  val list3 = Monad[List].map(list2)(a => a + 123)

  // The Monad Default Instances
  // Cats provides isntances for all the monads in the standard library (Option, List, Vector, ...)
  Monad[Option].flatMap(Option(1))(a => Option(a * 2))
  Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
  Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))

  // Cats provides a Monad for Future, but an `ExecutionContext` must be in scope!
  import cats.instances.future._
  // import scala.concurrent._
  // import scala.concurrent.duration._

  val fm = Monad[Future]
  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
  // val resFuture = Await.result(future, 1.second)

  // The Monad Syntax
  /*
   * cats.syntax.flatMap, cats.syntax.functor for `map`, cats.syntax.applicative for `pure`
   * Often they're all imported with `cats.implicits`
   * We can use `pure` to construct instances of a Monad
   *  We'll often need to specify the type params to disambiguate the particular instance we want
   */
  import cats.syntax.applicative._
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  1.pure[Option]
  1.pure[List]

  def sumSquaure[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))

  def alsoSumSqure[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  println(sumSquaure(Option(3), Option(4)))
  println(sumSquaure(List(1, 2, 3), List(4, 5)))

  // SECTION 4.3: THE IDENTITY MONAD
  // An issue with the above code is that sumSquare can't accept plain values
  // Cats provides `Id` type to abstract over monadic and non-monadic values
  import cats.Id

  println(sumSquaure(3: Id[Int], 4: Id[Int]))
  val a = Monad[Id].pure(3)
  val b = Monad[Id].flatMap(a)(_ + 1)
  val c =
    for {
      x <- a
      y <- b
    } yield x + y

  // EXERCISE 4.3.1: Monadic Secret Identities
  // Implement pure, map, and flatMap for Id.
  trait ExerciseId {
    def pure[A](value: A): Id[A] = value
    def map[A, B](init: Id[A])(func: A => B): Id[B] = func(init)
    def flatMap[A, B](init: Id[A])(func: A => Id[B]): Id[B] = func(init)
  }

  // SECTION 4.4: EITHER
  // `Either` pre-Scala 2.12 wasn't right-based
  // The Cats Either import can be imported or omitted depending on Scalac version

  // 4.4.2: Creating Instances
  import cats.syntax.either._

  // These "smart constructors" are helpful because they return
  // the results as type Either instead of Left or Right, which
  // can cause compiler type-inference bugs from over-narrowing
  val aRight = 3.asRight[String]
  val bRight = 4.asRight[String]
  for {
    x <- aRight
    y <- bRight
  } yield x * x + y * y

  // Cat's Either adds useful extension methods for exception handling
  Either.catchOnly[NumberFormatException]("foo".toInt)
  Either.catchNonFatal(sys.error("Badness"))

  // As well as extension methods for creating Eithers from other data types
  Either.fromTry(scala.util.Try("foo".toInt))
  Either.fromOption[String, Int](None, "badness")

  // 4.4.3: Transforming Eithers
  "Error".asLeft[Int].getOrElse(0)
  "Error".asLeft[Int].orElse(2.asRight[String])

  // `ensure` checks whether the right-hand value satisfies a predicate:
  -1.asRight[String].ensure("Must be non-negative!")(_ > 0)

  // `recover` and `recoverWith` provide similar error-handling to Future:
  "error".asLeft[Int].recover {
    case _: String => -1
  }

  "error".asLeft[Int].recoverWith {
    case _: String => Right(-1)
  }

  // `leftMap` and `bimap` are also implemented:
  "foo".asLeft[Int].leftMap(_.reverse)
  6.asRight[String].bimap(_.reverse, _ * 7)
  "bar".asLeft[Int].bimap(_.reverse, _ * 7)

  // `swap` lets us swap Left for Right
  123.asRight[String].swap

  // 4.4.4: Error Handling
  // Either is typically used to implement fail-fast error handling
  // If one computation fails, the remaining computations are not run:
  for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <- if (b == 0) "DIV0".asLeft[Int]
    else (a / b).asRight[String]
  } yield c * 100

  // A good pattern for error-handling is to define Algebraic Data Types
  // to represent errors that may occur:
  sealed trait LoginError extends Product with Serializable
  final case class UserNotFound(username: String) extends LoginError
  final case class PasswordIncorrect(username: String) extends LoginError
  case object UnexpectedError extends LoginError
  case class User(username: String, password: String)
  type LoginResult = Either[LoginError, User]

  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u)      => println(s"User not found: $u")
      case PasswordIncorrect(u) => println(s"Password incorrect: $u")
      case UnexpectedError      => println(s"Unexpected error")
    }

  val errResult1: LoginResult = User("dave", "passw0rd").asRight
  val errResult2: LoginResult = UserNotFound("dave").asLeft

  errResult1.fold(handleError, println)
  errResult2.fold(handleError, println)

  // EXERCISE 4.4.5: What is Best?
  // What other features might we want from error handling for the last example?
  /*
   * Error recovery is important when processing large jobs.
      We don’t want to run a job for a day and then find it failed on the last element.
   * Error reporting is equally important.
      We need to know what went wrong, not just that something went wrong.
   * In a number of cases, we want to collect all the errors, not just the first one we encountered.
   * A typical example is validating a web form.
   *  It’s a far better experience to report all errors to the user when they submit a form
      than to report them one at a time.
   */

  // SECTION 4.5: Error Handling and MonadError
  // Cats provides a type class MonadError that abstracts over Either-like DTs
  // MonadError Type Class
  import cats.MonadError
  import cats.instances.either._

  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]
  val success = monadError.pure(42)
  val failure = monadError.raiseError("Badness")

  // Raising and Handling Errors
  // The two most important methods of MonadError are `raiseError` and `handleError`
  // `raiseError` is like `pure` except that it creates an inst. repr. a failure
  val success1 = monadError.pure(42)
  val failure1 = monadError.raiseError("Badness")

  // `handleError` allows us to consume an error and (possibly) turn it into a Success
  // similar to `recover` method for Future
  monadError.handleError(failure) {
    case "Badness" => monadError.pure("Its ok")
    case _         => monadError.raiseError("Its not okay")
  }

  // `ensure` implements filter-like behavior with predicate logic
  monadError.ensure(success)("Number too low")(_ > 1000)

  // SECTION 4.6: The Eval Monad
  /*
   * `cats.Eval` is monad that allows us to abstract over different models of computation.
   * There's eager and lazy computation, and (non-)memoized results.
   * `Eval` has three subtypes: `Now`, `Later`, and `Always`
   */
  import cats.Eval

  val now = Eval.now(math.random + 1000) // eager, memoized
  val later = Eval.later(math.random + 2000) // lazy, memoized
  val always = Eval.always(math.random + 3000) // lazy, not memoized
  println((now.value, later.value, always.value))

  // While semantics of `Eval` instances are maintained, `map`-ed functions
  // are always called lazily on-demand (`def` semantics)
  val ans = for {
    a <- Eval.now { println("Calculating A"); 40 }
    b <- Eval.always { println("Calculating B"); 2 }
  } yield {
    println("Adding A and B")
    a + b
  }
  println(f"First access: ${ans.value}")
  println(f"Second access: ${ans.value}")

  // Eval has a `memoize` method that allows us to memoize a chain of computations.
  // The result of the chain is cached up to the point where memoize is called
  // After `memoize` calculations will retain their original semantics
  val saying = Eval
    .always { println("Step 1"); "The cat" }
    .map { str => println("Step 2"); s"$str sat on" }
    .memoize
    .map { str => println("Step 3"); s"$str the mat" }
  println(f"First access: ${saying.value}")
  println(f"Second access: ${saying.value}")

  // 4.6.4 Trampolining and `Eval.defer`
  // "Trampolining" allows for nested calls to map and flatMap that won't consume extra stack frames.
  // "Trampolining" allows for stack safety. This is unsafe code: (e.g. factorial(50000))
  def factorial(n: BigInt): BigInt =
    if (n == 1) n else n * factorial(n - 1)

  // Still unsafe: This makes recursive calls before working with Eval's `map`
  def unsafeFactorial(n: BigInt): Eval[BigInt] =
    if (n == 1) Eval.now(n) else unsafeFactorial(n - 1).map(_ * n)

  // We can work around this using `Eval.defer`
  // It takes an existing Eval instance and defers its evaluation
  // `defer` is trampolined too
  def safeFactorial(n: BigInt): Eval[BigInt] =
    if (n == 1) Eval.now(n) else Eval.defer(safeFactorial(n - 1).map(_ * n))

  println(safeFactorial(50).value) // A Long could be used here if desired

  // EXERCISE 4.6.5: Safer Folding Using Eval
  // Make the naive implementation of foldRight stack safe using Eval:
  def oldFoldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    as match {
      case head :: tail => fn(head, oldFoldRight(tail, acc)(fn))
      case Nil          => acc
    }

  def evalFoldRight[A, B](as: List[A], acc: Eval[B])(
      fn: (A, Eval[B]) => Eval[B]
  ): Eval[B] =
    as match {
      case head :: tail => Eval.defer(fn(head, evalFoldRight(tail, acc)(fn)))
      case Nil          => acc
    }

  // SECTION 4.7: The Writer Monad
  // `cats.data.Writer` monad allows us to carry a log with our computations.
  // The log can record messages, errors, and additional data about a computation.
  // A common use of Writers is recording Seqs of steps in multi-threaded computations
  // where standard logging can lead to interleaved messages from many contexts
  // With Writer's log we can run concurrent computations without mixing logs
  import cats.data.Writer

  Writer(
    Vector(
      "It was the best of times",
      "It was the worst of times"
    ),
    1859
  )

  // Cats provides a way of creating Writers specifying only the log or result.
  // We must have a Monoid[W] in scope so Cats knows how to produce an empty log:
  import cats.syntax.applicative._

  type Logged[A] = Writer[Vector[String], A]
  123.pure[Logged]

  // If we have a log and no result, create a Writer[Unit] using `tell` syntax
  import cats.syntax.writer._

  Vector("msg1", "msg2", "msg3").tell

  // If we have a result and log, use Writer.apply or the writer syntax
  val aW = Writer(Vector("msg1", "msg2", "msg3"), 123)
  val bW = 123.writer(Vector("msg1", "msg2", "msg3"))

  val aResult: Int = aW.value
  val aLog: Vector[String] = aW.written

  val (log, result) = bW.run

  // Section 4.7.2: Composing and Transforming Writers
  // Its good practice to use log types with efficient append and concat, like Vector
  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  println(writer1)

  // Besides transforming the result with map and flatMap, we can transform
  // the log into a Writer with the `mapWritten` method:
  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  println(writer2.run)

}
