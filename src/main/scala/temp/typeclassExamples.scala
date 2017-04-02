package temp

import temp.typeclassExamples.toHtml


object typeclassExamples {
  trait HtmlWriter[A] {
    def write (value: A): String
  }

  def toHtml[A: HtmlWriter](value: A): String =
    implicitly[HtmlWriter[A]].write(value)


  object HtmlWriter {
    def apply[A](func: A ⇒ String) = new HtmlWriter[A] {
      def write(value: A): String = func(value)
    }
  }

  case class Email(address: String)
  case class Person (name: String, email: Email, age: Int)

  implicit val emailWriter: HtmlWriter[Email] =
    HtmlWriter((email: Email) => email.address.replaceAll("@", " at "))

  implicit val personWriter: HtmlWriter[Person] =
    HtmlWriter { (person: Person) =>
      toHtml(person.name) + " " +
        toHtml(person.email) + " " +
        toHtml(person.age)
    }

  implicit val intWriter: HtmlWriter[Int] =
  HtmlWriter((value: Int) => value.toString)

  implicit val stringWriter: HtmlWriter[String] =
    HtmlWriter((value: String) => value.replaceAll("<", "&lt;").replaceAll(">", "&gt;"))


  toHtml(Person("Dave", Email("dave@example.com"), 36))
  toHtml(123)(intWriter)
}




object typeClassExamplesApp extends  App {

  import typeclassExamples._

  println(toHtml(Person("Dave", Email("dave@example.com"), 36)))
  println(toHtml(124)(intWriter))
}