package essenceoffp


object functors01 extends  App {

  trait List[A]  {
    def map[B] (f: A ⇒ B): List[B]
  }


  trait Option[A] {
    // map for Option[A]
    def map[B] (f: A ⇒ B): Option[B]
  }


  trait Functor[F[_]] {
    def map[A, B](f : A => B): F[A] => F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {

    def apply[A, B](f: F[A => B]): F[A] => F[B]

    def pure[A](a: => A): F[A]
  }
}


object functors02 extends App {

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._


  def identity[A](x: A): A         = x

  Option(1).map(identity) shouldEqual Option(1)
  None.map(identity) shouldEqual None

}

object functors03 extends App {

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._


  //normalizes string
  val f: String => String = _.replaceAll("ß", "ss").replaceAll("[^a-zA-Z0-9-]+", "-")

  //checks if a string is a palindrome
  val g: String => Boolean = s => s == s.reverse

  // compose f and g with `andThen`
  val h: String => Boolean = f >>> g // >>> == andThen



  val lst  = List("madam", "ßßß")

  val l1 = lst.map(f).map(g)

  val l2 = lst.map(h) // ← f andThen g

  l1 should contain theSameElementsAs l2 // ← List(true, true)
}

object functorTreeExample01 extends App {
  import org.scalatest.Matchers._
  import scalaz._
  import Scalaz._

  sealed trait Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  implicit val TreeFunctor = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
      case Leaf(a: A) => Leaf(f(a))
      case Node(left, right) => Node(map(left)(f), map(right)(f))
    }
  }


  val f: String => String = _.replaceAll("ß", "ss").replaceAll("[^a-zA-Z0-9-]+", "-")

  val binaryTree =
    Node(
      Leaf("scala"),
      Node(
        Leaf("eXch;nge"),
        Leaf("ßß")))


  val transformedTree: Tree[String] =  Functor[Tree].map(binaryTree)(f) // f normalizes string

  transformedTree shouldEqual
    Node(
      Leaf("scala"),
      Node(
        Leaf("eXch-nge"),
        Leaf("ssss"))) // ← a tree with  normalized strings

}