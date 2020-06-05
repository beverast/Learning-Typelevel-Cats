package sandbox

import cats.Monoid
import cats.Semigroup

import cats.syntax.semigroup._  // for |+|

import cats.instances.string._
import cats.instances.int._
import cats.instances.option._

object CatsMonoids {
  Monoid[String].combine("Hi ", "there")
  Monoid[String].empty

  // Equivalent to...
  Monoid.apply[String].combine("Hi ", "there")
  Monoid.apply[String].empty

  // If we don't need `empty`
  Semigroup[String].combine("Hi ", "there")

  // Ints and Option[Int]
  Monoid[Int].combine(32, 10)
  val a = Option(22)
  val b = Option(20)
  Monoid[Option[Int]].combine(a, b)

  // `combine` operator
  val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
  val intResult = 1 |+| 2 |+| 3

  // EXERCISE 2.5.4: ADDING ALL THE THINGS
  def add[A: Monoid](items: List[A]): A =
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  implicit val monoid: Monoid[Order] = new Monoid[Order] {
    def combine(o1: Order, o2: Order) =
      Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity)

    def empty = Order(0, 0)
  }

  def addAll[A](values: List[A])(implicit monoid: Monoid[A]): A =
    values.foldLeft(monoid.empty)(_ |+| _)
}