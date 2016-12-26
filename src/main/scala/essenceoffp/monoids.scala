package essenceoffp

import scalaz.Monoid


object monoidsExample01 extends App {

  trait Semigroup[A] {
    def append(a1: A, a2: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def zero: A
  }

}


object monoidsExample02 extends  App {

  import scalaz._
  import Scalaz._

  val stringMonoid = new Monoid[String] {
    val zero = ""
    def append(a1: String, a2: ⇒ String) = a1 + a2
  }

  def optionMonoid[A](implicit  m: Monoid[A]) = new Monoid[Option[A]]{

    def zero: Option[A] = None

    def append(f1: Option[A], f2: => Option[A]): Option[A] =
      f1 match {
        case None ⇒ None
        case Some(a) ⇒ f2 match {
          case None ⇒ None
          case Some(b) ⇒ Option(m.append(a, b))
        }
      }
  }
}

object monoidsExample03 extends App {
  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  def reduce[A](list: List[A]) (implicit  m: Monoid[A]): A =
    list match {
      case Nil => m.zero
      case x :: xs => m.append(x, reduce(xs))
    }


    val result = reduce(List(1,2,3))
    reduce(List("a","b","c")).println

    result.println

}

object monoidsExample04 extends App {
  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  def reduce[A](list: List[A]) (implicit  m: Monoid[A]): A =
    list match {
      case Nil => m.zero
      case x :: xs => m.append(x, reduce(xs))
    }

  reduce(List(1,2,3)) shouldEqual 6

  reduce(List("a","b","c")) shouldEqual "abc"

  reduce(List(Option(1), Option(2), Option(3))) shouldEqual Option(6)


}