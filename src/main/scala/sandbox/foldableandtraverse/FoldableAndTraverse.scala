package sandbox.foldableandtraverse

object FoldableAndTraverse {
  // We'll look at two type classes that caputre iteration over collections:
  // Foldable abstracts over foldLeft and foldRight
  // Traverse is a higher-level abstraction that uses Applicatives
  //    to iterate with less pain than folding.
  // We'll start with Foldable, then examine cases where folding becomes complex
  //    and Traverse becomes more convenient.

  // SECTION 7.1: Foldable
  /*
   * Using Foldable, we can write generic folds that work with a variety of seq types.
   * We can also create new sequences and plug them into our code.
   * Foldable gives us great use cases for Monoids and the Eval monad.
   */

  // Quick recap of the general concept of folding:
  // We supply an "accumulator" value and a "binary function" to combine it
  //    with each item in the sequence:
  def show[A](list: List[A]): String =
    list.foldLeft("nil")((accum, item) => s"$item then $accum")

  println(show(Nil))
  println(show(List(1, 2, 3)))

  // foldLeft works recursively down the seq, our binary func. is called for each item
  // and the result of each call becoming the accumulator for the next.
  // When we reach the end of the seq, the final accumulator becomes our final result.
  // Fold order can be important so we have:
  //    foldLeft: "left" to "right" (start to finish)
  //    foldRight: "right" to "left" (finish to start)
  println("foldLeft and foldRight are equiv. with associate binary functions")
  println(List(1, 2, 3).foldLeft(0)(_ + _))
  println(List(1, 2, 3).foldRight(0)(_ + _))

  // EXERCISE 7.1.2: Reflecting on Folds
  // Try using foldLeft and foldRight with an empty list as the accumulator and
  // :: as the binary operator. What reults do get in each case?
  println(List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a))
  println(List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a))
}
