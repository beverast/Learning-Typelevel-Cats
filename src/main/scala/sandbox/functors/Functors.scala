package sandbox.functors

  import cats.instances.function._
  import cats.syntax.functor._

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

}