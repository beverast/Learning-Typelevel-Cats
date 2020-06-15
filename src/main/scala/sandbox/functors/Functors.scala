package sandbox.functors

import cats.instances.function._
import cats.syntax.functor._
// import cats.data.Func
import cats.Functor

object Functors {
  /*
   * Informally, functor: anything with a `map` method
   * `map` ensures it is applied to every item in a functor (i.e., `List[T]`)
   * `map` over an `Option` leaves the `Some` or `None` contexts unchanged
   * "     "    "  `Either` leaves the `Left` and `Right` contexts unchanged
   * `map` of `List`, `Option`, and `Either` apply functions eagerly
   */
  println(List(1, 2, 3).map(n => n + 1))
  println(
    List(1, 2, 3)
      .map(n => n + 1)
      .map(n => n * 2)
      .map(n => s"${n}!")
  )

  // Functors are also single-arg functions
  // Start with `X => A`, supply `A => B`, get back `X => B`
  // Mapping over a function is function composition
  val func1: Int => Double =
    (x: Int) => x.toDouble

  val func2: Double => Double =
    (y: Double) => y * 2

  // composition using `map`
  println(
    (func1 map func2)(1)
  )
  // composition using andThen
  println(
    (func1 andThen func2)(1)
  )

  // Calling `map` doesn't run any operations unless
  // an argument is passed to the final function
  // Then all of the operations are run in sequence.
  // `map` is a lazy queue of operations
  val func =
    ((x: Int) => x.toDouble)
      .map(x => x + 1)
      .map(x => x * 2)
      .map(x => s"${x}!")
  println(func) // Prints a function object

  // SECTION 3.3 Definition of a Functor
  // cats encodes `Functor` as a type class: `cats.Functor`
  // Functor Laws:
  // Identity: fa.map(a => a) == fa
  // Composition: fa.map(g(f(_)))

  // SECTION 3.4: Higher Kinds and Type Constructors
  // `List` is a type constructor, takes one param
  // `List[A]` is a type, produced using a type param.
  // Declare type constructors using underscores

  // SECTION 3.5: Functors in Cats
  import cats.instances.list._
  import cats.instances.option._
  val list1 = List(1, 2, 3)
  val list2 = Functor[List].map(list1)(_ * 2)
  val option1 = Option(123)
  val option2 = Functor[Option].map(option1)(_.toString)

  // `Functor` also provides the `lift` method, which converts a function
  // of type `A => B` to one that operates over a functor and has type `F[A] => F[B]`
  val funcOne = (x: Int) => x + 1
  val liftedFunc = Functor[Option].lift(funcOne)
  println("liftedFunc: " + s"${liftedFunc(Option(1))}")

  // scalac will alwways prefer built-in methods, like `map` for Options and Lists
  // Abstracting over functors: this method applies an equation no matter what
  // functor context it is in.
  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  import cats.instances.option._
  import cats.instances.list._

  doMath(Option(20))
  doMath(List(1, 2, 3))

  // Example: Define a functor by defining its map method
  // Here's a functor for Option. An impl. exists in cats.instances.
  implicit val optionFunctor: Functor[Option] =
    new Functor[Option] {
      def map[A, B](value: Option[A])(func: A => B): Option[B] =
        value.map(func)
    }

  // Sometimes we need to inject dependencies into our instances.
  // Example: Inject the necessary ExecutionContext into `futureFunctor`
  import scala.concurrent.{Future, ExecutionContext}

  implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] =
    new Functor[Future] {
      def map[A, B](value: Future[A])(func: A => B): Future[B] =
        value.map(func)
    }

  // Exercise 3.5.4: Branching Out with Functors
  // Write a `Functor` for the following binary tree data type:
  sealed trait Tree[+A]

  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](value: Tree[A])(func: A => B): Tree[B] =
        value match {
          case Branch(left, right) =>
            Branch(map(left)(func), map(right)(func))
          case Leaf(value) =>
            Leaf(func(value))
        }
    }

  // We need to create some smart constructors to compensate
  // for the compiler not being able to find a Functor inst. for Branch or Leaf
  object Tree {
    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)
    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  // Test `treeFunctor` to transform some `Trees`
  println(Tree.leaf(100).map(_ * 2))
  println(Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2))

  // SECTION 3.6: Contravariant and Invariant Functors

}
