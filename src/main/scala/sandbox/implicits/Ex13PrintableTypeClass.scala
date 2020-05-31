package sandbox.implicits

/*
1. Define a type class `Printable[A]` containing a single method `format`.
   `format` should accept a value of type `A` and return a `String`.
2. Create an object `PrintableInstances` containing instances of `Printable`
   for `String` and `Int`.
3. Define an object `Printable` with two generic interface methods:
    - `format()` accepts a value of type `A` and a `Printable` of the corresponding type.
      It uses the relevant `Printable` to convert the `A` to `String`.
    - `print()` accepts the same parameters as `format` and returns `Unit`.
       It prints the `A` value to the console using `println()`.
4. Exercise 1.3: Implement a `Printable` `Cat` which returns:
   NAME is a AGE year-old COLOR cat. Print the cat.
 */

case class Cat(name: String, age: Int, color: String)

trait Printable[A] {
  def format(value: A): String
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  def print[A](value: A)(implicit p: Printable[A]): Unit =
    println(format(value))
}

object PrintableInstances {
  implicit val printableString: Printable[String] =
    new Printable[String] {
      override def format(value: String): String = value
    }

  implicit val printableInt: Printable[Int] =
    new Printable[Int] {
      override def format(value: Int): String = value.toString
    }

  implicit val printableCat: Printable[Cat] =
    new Printable[Cat] {
      override def format(cat: Cat): String =
        f"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
    }
}

object PrintableSyntax {
  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)

    def print(implicit p: Printable[A]): Unit =
      println(format(p))
  }
}