package sandbox

//import cats._
//import cats.implicits._

object MonoidSemigroupNotes {
  /*
   - `monoid` and `semigroup` are type classes that allow for adding and combining
   - There are instances for Ints, Strings, Lists, Options, and more.
   - Integer addition is "closed" meaning adding two `Int`s always produces another `Int`
   - There is an "identity" element, 0, such that a + 0 == 0 + a == a
   - Ints have "associativity": order doesn't matter.
   - The same properties also apply for for multiplication, but use 1 for identity.
   - Subtraction is not a monoid because its not associative: (1-2)-3 != 1-(2-3)
   */
  assert((2+1).isValidInt) // Result type is Int
  assert((2+0) == 2) // Identity element
  assert((1 + 2 + 3) == ((2 + 3) + 1))  // Associativity
  assert(((1 * 2) * 3) == (3 * 2 * 1)) // Associative multiplication

  // String and sequence concatenation
  assert(("One" ++ "two") == "Onetwo")  // res: String = "Onetwo"
  assert(("" ++ "Hello") == ("Hello" ++ ""))   // An empty string is the identity element
  assert((("One" ++ "Two") ++ "Three") == ("One" ++ "Two" ++ "Three"))

  // SECTION 2.1: DEFINITION OF A MONOID
  /*
  Formally, a monoid for a type `A` must:
    - Have an operation `combine` with type (A, A) => A
    - Have an identity element `empty` of type A
    - Be associative
   */
  trait Monoid[A] {
    def combine(x: A, y: A): A
    def empty: A
  }

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

  // SECTION 2.2: DEFINITION OF A SEMIGROUP
  /*
  - A semigroup is the `combine` part of a monoid.
  - Many semigroups are monoids, but for some types we cannot define an `empty` element.
  - Cats has a `NonEmptyList` which has a semigroup implementation but no Monoid impl.
   */
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait MonoidAgain[A] extends Semigroup[A] {
    def empty: A
  }

  // EXERCISE 2.3: THE TRUTH ABOUT MONOIDS
  // Define monoids for the `Boolean` type
  object Monoid {
    def apply[A](implicit monoid: Monoid[A]) = monoid
  }

  implicit val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = a && b
      def empty = true
    }

  implicit val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = a || b
      def empty = false
    }

  implicit val booleanEitherMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = (a && !b) || (!a && b)
      def empty = false
    }

  implicit val booleanXnorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = (!a || b) && (a || !b)
      def empty = true
    }

  // EXERCISE 2.4: ALL SET FOR MONOIDS
  // What monoids and semigroups are there for sets?
  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a union b
      def empty = Set.empty[A]
    }

  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = a intersect b
    }

  implicit def symDiffMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def combine(a: Set[A], b: Set[A]) = (a diff b) union (b diff a)
      def empty: Set[A] = Set.empty
    }

  // SECTION 2.5: MONOIDS IN CATS

}
