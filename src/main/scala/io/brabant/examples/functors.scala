package io.brabant.examples

import scalaz._, Scalaz._
import org.scalatest._
import Matchers._

object example01 {

 def maxValue(xs: List[Int]): Int =
   xs.max

  def divide100By(x: Int): Int =
    100 / x
}

object example02{
  def divide100By(x: Int): Option[Int] = x match {
    case 0 ⇒ None
    case _ ⇒ Some(100 / x)
  }
}

object example03 {

  def maxValue(xs: List[Int]): Error \/ String = ???
}

object exampleApplicative01{

  def apply[A, B](oFunc : Option[A => B]) : Option[A] => Option[B] = {
    case None => None
    case Some(a) => oFunc.map(f => f(a))
  }
}


object exampleMonoid01 extends App{

  val zeroList = Monoid[List[Int]].zero
  val zeroString = Monoid[String].zero

  val stringConcat = "one" |+| "two"
  val listConcat = List(1, 2, 3) |+| List(4, 5, 6)


}


object exampleMonoid02 extends App {

  def reduce[A](list: List[A]) (implicit  m: Monoid[A]): A =
    list match {
      case Nil => m.zero
      case x :: xs => m.append(x, reduce(xs))
    }


  val result = reduce(List(1,2,3))
  reduce(List("a","b","c")).println

  result.println


}

object exampleTraversable01 extends App{
  val opts: List[Option[Int]] = List(Option(1), Option(2), Option(3))

  val result: Option[List[Int]] = opts.sequence

  println(result)
}

object exampleMonoid03 extends App {
  def sumGeneric[F[_], A](fa: F[A])(implicit F: Foldable[F], A: Monoid[A]): A =
    fa.fold


  sumGeneric(List(1,2,4)).println
  sumGeneric(List(1.some, 2.some)).println
  sumGeneric(List(1.some, None)).println
}

object exampleFunctor01 extends App {
  implicit class LiftAsFunctor[M[_]: Functor, A](m: M[A]){
    def mapWith[B](f: A ⇒ B): M[B] =
      implicitly[Functor[M]].map(m)(f)
  }

  val result1 = List(1, 2) mapWith (_ + 1)
  val result2 = List("a", "b") mapWith (_ + "c")
  val result3 = 1.some mapWith (_ * 10)


  result3.println

  import org.scalatest.Matchers._

  val f: String => String = _.replaceAll("ß", "ss").replaceAll("[^a-zA-Z0-9-]+", "-")
  def divideOneBy(x: Int): Double = 1/x.toDouble

  List("scala","eXch;nge").mapWith(f) should contain theSameElementsAs List("scala", "eXch-nge")

  Option(2).mapWith(divideOneBy) shouldEqual Some(.5)

  import scalaz._
  import Scalaz._

}

object FunctorExample2 extends App {
  sealed trait Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  implicit  val treeFunctor = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match{
      case Leaf(a: A) => Leaf(f(a))
      case Node(left, right) => Node (map(left)(f), map(right)(f))
    }
  }


  val tree = Node(Leaf(1), Leaf(2))

  val functor = implicitly[Functor[Tree]]

  val result: Tree[Boolean] = Functor[Tree].map(tree)(i => i%2 == 0)

  println(result)



  val tree2 = Node (Leaf("m;dam"), Leaf("ßß"))

  val f: String => String =
    _.replaceAll("ß", "ss")
      .replaceAll("[^a-zA-Z0-9-]+", "-")


  Functor[Tree].map(tree2)(f) |> println


}


object FunctorExample3 extends App {
  val f1: String => String =
     _.replaceAll("ß", "ss")
      .replaceAll("[^a-zA-Z0-9-]+", "-")

  val f2: String => Boolean = s => s == s.reverse

  val f: String => Boolean = f1 andThen f2

    val l1 = List("madam", "ßßß").map(f1).map(f2)
    val l2 = List("madam", "ßßß").map(f)  // <- List(true, true)

  println(l2)
  println(l1

  )

  l1 should contain theSameElementsAs l2
}


