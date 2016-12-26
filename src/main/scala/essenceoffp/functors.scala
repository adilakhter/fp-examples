package essenceoffp

import essenceoffp.applicativesExample04.ApplicativeOps
import essenceoffp.example01.IO

import scalaz.Functor

object functorsExampleSetIsNotFunctor extends  App {

  case class Container[A](unwrap: A) {
    override def equals(obj: scala.Any): Boolean = true
  }

  val f = (i: Int) ⇒ Container(i)
  val g = (wrap: Container[Int]) ⇒ wrap.unwrap

  val aSet = Set(1,2,3)

  aSet.map(f).map(g) // Set(1)

  aSet.map(f andThen g) // Set(1,2,3 )


}


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

object FunctorExample04 extends App{


  import scalaz._
  import Scalaz._

  final case class IO[A](unsafePerformIO: () => A)

  def readLine2: IO[String] = IO(() => readLine())

  def println2(s: String): IO[Unit] = IO(() => println(s))

  def io[A](computation: =>A): IO[A] = IO(() => computation)



  implicit val IOFunctor: Functor[IO] = new Functor[IO]{
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] =
      IO (() ⇒ f(fa.unsafePerformIO()))
  }


  val x = IO(() ⇒ "100.0")
  val y: IO[Double] = x.map(_.toDouble)

  println(y.unsafePerformIO())

}

object ApplicativeExample05 extends App{


  import scalaz._
  import Scalaz._

  final case class IO[A](unsafePerformIO: () => A)

  def readLine2: IO[String] = IO(() => readLine())

  def println2(s: String): IO[Unit] = IO(() => println(s))

  def io[A](computation: =>A): IO[A] = IO(() => computation)



  implicit val IOApplicative = new Applicative[IO]{
    def point[A](a: => A): IO[A] = IO(() ⇒ a)

    def ap[A, B](fa: => IO[A])(f: => IO[A => B]): IO[B] = {
      IO(() ⇒ f.unsafePerformIO().apply(fa.unsafePerformIO()))
    }
  }

//  case class ApplicativeOps[F[_]: Applicative,A] (self: F[A]) {
//    val  F = implicitly[Applicative[F]]
//
//    def <*>[B](f: F[A => B]): F[B] = F.ap(self)(f)
//  }
//
//  implicit def ToApplicativeOps[F[_]: Applicative, A](v: F[A]): ApplicativeOps[F, A] =
//    ApplicativeOps(v)
//

  val add = (x:Double) ⇒ (y: Double) ⇒ x + y

  val longitudeIO = IO(() ⇒ "1.0")

  val longitude: IO[Double] = longitudeIO.map(_.toDouble)
  val latitude: IO[Double] = longitudeIO.map(_.toDouble)

  val result: IO[Double] = longitude <*> (latitude <*> add.point[IO])


  println(result.unsafePerformIO())


}