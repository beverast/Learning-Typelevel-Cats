package sandbox.monadtransformers

object MonadTransformers {
  // SECTION 5.1: Composing Monads
  // Given two arbitrary monads, can we combine them
  // in some way to make a single monad?
  import cats.data.OptionT
  import cats.instances.list._
  import cats.syntax.applicative._

  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]

  println(
    result1.flatMap { (x: Int) => result2.map { (y: Int) => x + y } }
  )

  // SECTION 5.3: Monad Transformers in Cats
  /*
   * Each monad transformer is a data type, defined in `cats.data`
   * These allow us to wrap stacks of monads to produce new monads
   * By convention, a monad Foo will have a transformer class FooT
   * Many monads in Cats are defined by combining a monad transformer with the Id monad
   * There's `cats.data.{ OptionT, EitherT, ReaderT, WriterT, StateT, IdT }` among others
   *
   * SECTION 5.3.2: Building Monad Stacks
   * The Transformer represents the "inner" monad in a stack, while the
   * first type parameter specifies the outer monad. The remaining type parameters are
   * the types we've used to form the corresponding monads.
   *
   * For example, our ListOption type is an alias for OptionT[List, A] but the result
   * is effectively a List[Option[A]]. We build monad stacks from the inside out.
   */

  type ListOption2[A] = OptionT[List, A]

  // Alias Either to a type constructor with one parameter:
  type ErrorOr[A] = Either[String, A]

  // Build our final monad stack using OptionT:
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  import cats.instances.either._ // for Monad

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c = a.flatMap(x => b.map(y => x + y))

  // Things get more confusing when stacking 3+ monads
  // F[_] is the outer monad in the stack (Either is the inner)
  // E is the error type for the Either
  // A is the result type for the Either
  // case class EitherT[F[_], E, A](stack: F[Either[E, A]])

  // Let's create an alias for EitherT that fixes Future and Error
  // but allows A to vary
  import cats.data.EitherT
  import scala.concurrent.Future

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  import cats.instances.future._ // for Monad
  import scala.concurrent.ExecutionContext.Implicits.global

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b

  // SECTION 5.3.3: Constructing and Unpacking Instances
  // We can create transformed monad stacks using the transformer's apply method
  // or the usual pure syntax.
  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
  val errorStack2 = 32.pure[ErrorOrOption]

  // Now we can unpack it using its value method, returning the untransformed stack.
  println(errorStack1.value)
  println(errorStack2.value.map(_.getOrElse(-1)))

  // Each call to value unpacks a single monad transformer
  // We may need more than one call to completely unpack a large stack
  // For example, the `Await` the `FutureEitherOption` stack above
  val intermediate = futureEitherOr.value
  val stack = intermediate.value

  // SECTION 5.3.5: Usage Patterns
  // A common pattern is to use monad transformers at boundaries of contexts as glue code.
  // This will allow each module of code to make its own decisions about which transformers to use:
  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]

  // Methods generally return untransformed stacks
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None      => Writer(List(s"Failed on $str"), None)
    }

  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT

    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c

    result.value
  }

  // This approach doesn't force OptionT on other users' code:
  println(addAll("1", "2", "3"))
  println(addAll("1", "a", "3"))

  // EXERCISE 5.4: Monads: Transform and Roll Out
  import cats.data.EitherT
  import scala.concurrent.Future

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(ally: String): Response[Int] = {
    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10
    )

    powerLevels.get(ally) match {
      case Some(avg) => EitherT.right(Future(avg))
      case None      => EitherT.left(Future(s"$ally unreachable"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield (power1 + power2) > 15

  // def tacticalReport(ally1: String, ally2: String): String = {
  //   val stack = canSpecialMove(ally1, ally2).value
  //   stack match {
  //     case Left(msg)    => s"Comms error: $msg"
  //     case Right(true)  => s"$ally1 and $ally2 are ready to roll out!"
  //     case Right(false) => s"$ally1 and $ally2 need a recharge."
  //   }
  // }

  // SECTION 5.5: Summary
  /*
 * Monad transformers eliminate the need for nested for commprehensions
 *  and pattern matching when working with "stacks" of nested monads.
 * Each monad transformer (FutureT, OptionT, EitherT) provides the code needed
 *  to merge its related monad with other monads.
 * The type signatures of monad transformers are written from the inside out, so an
 *  EitherT[Option, String, A] is a wrapper for an Option[Either[String, A]]
 *
 */
}
