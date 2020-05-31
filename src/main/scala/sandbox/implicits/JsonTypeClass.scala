package sandbox.implicits

// A "type class" is an interface or API that represents some functionality.
// In Cats a type class is represented by a trait with at least one type parameter.

// Define a very simple JSON AST
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
final case object JsNull extends Json

// The "serialize to JSON" behavior is encoded in this trait
// `JsonWriter` is the type class, with `Json` and its subtypes providing supporting code.
trait JsonWriter[A] {
  def write(value: A): Json
}

// A type class interface is any functionality we expose to users.
// Interfaces are generic methods that accept instances of the type class as implicit parameters.
// There are two common ways of specifying an interface: Interface Objects and Interface Syntax.
// The simplest way of creating an interface is to place methods in a singleton object.
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

// Alternatively, we can use "extension methods" to extend existing type classes
// with interface methods. Cats refers to this as "syntax" for the type class
object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}