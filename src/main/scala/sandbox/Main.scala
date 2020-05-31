package sandbox

import java.util.Date

import cats._
import cats.implicits._
import sandbox.implicits.JsonWriterInstances._
import sandbox.implicits.JsonSyntax._
import sandbox.implicits.PrintableInstances._
import sandbox.implicits.PrintableSyntax._
import sandbox.implicits.{Cat, Json, JsonWriter, Person, Printable}

object Main extends App {
  println("Hello " |+| "Cats!")

  // START IMPLICITS: 1.0 - 1.3
  // The compiler searches for type class instances of the relevant type and
  // inserts them at the call site:
  println(Json.toJson(Person("Dave", "dave@protonmail.com")))
  // Interface syntax, compiler infers: .toJson(personWriter)
  println(Person("Dave", "dave@protonmail.com").toJson)

  // We can use `implicitly` to summon any value from implicit scope
  // We provide the type we want and `implicitly` does the rest
  println(implicitly[JsonWriter[String]])

  // Exercise 1.3
  val cat = Cat("Blue", 4, "Gray")
  Printable.print(cat)
  cat.print

  // START MEET CATS: 1.4
  // `Show` type class  is Cats' equivalent to `Printable`
  val showInt:    Show[Int]    = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]
  val intAsString: String    = showInt.show(123)
  val stringAsString: String = showString.show("abc")
  println((intAsString, stringAsString))
  // Interface syntax as imported from cats.syntax.show._
  val shownInt = 123.show
  val shownString = "abc".show
  println((shownInt, shownString))
  // Cats provides methods to simplify creating custom instances (of Show)
  implicit val dateShow: Show[Date] =
    Show.show(date => s"${date.getTime}ms since the epoch.")
  // Exercise 1.4 Re-implement the `Cat` application using `Show`
  implicit val catShow: Show[Cat] =
    Show.show(cat => s"${cat.format}")
  println(Cat("Linus", 9, "black").show)

  // START EXAMPLE: `Eq` 1.5
  // `Eq` supports Type-Safe Equality, interface syntax: === and =!=
  val eqInt = Eq[Int]
  eqInt.eqv(123, 123) // Boolean = true
  eqInt.eqv(123, 124) // Boolean = false
  123 === 123         // Boolean = true
  123 =!= 124         // Boolean = true
  // We have to "re-type" arguments in some cases
  (Some(1): Option[Int]) === (None: Option[Int])
  // We can shorten this with stdlib's Option methods
  Option(1) === Option.empty[Int]
  // Or via Cats' option syntax
  1.some === none[Int]  // Boolean = false
  1.some =!= none[Int]  // Boolean = true
  // We can define our own instances of `Eq` using the `Eq.instance` method
  // It accepts a function of type (A, A) => Boolean and returns an `Eq[A]`
  implicit val dateEq: Eq[Date] = {
    Eq.instance[Date] { (date1, date2) =>
      date1.getTime === date2.getTime
    }
  }
  val x = new Date()  // now
  val y = new Date()  // a bit later than now
  x === x             // Boolean = true
  x === y             // Boolean = false
  // Exercise 1.5: Implement an instance of `Eq` for `Cat`
  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      (cat1.name  === cat2.name ) &&
      (cat1.age   === cat2.age  ) &&
      (cat1.color === cat2.color)
    }
  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  println((cat1 === cat2).show)
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]
  println((optionCat1 === optionCat2).show)
}
