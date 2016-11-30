package essenceoffp

import org.scalatest._
import Matchers._

import scalaz._
import Scalaz._


object TreeImpl {

  sealed trait Tree[+A] {

    def insert[B >: A](elem: B)(implicit ev: B => Ordered[B]): Tree[B]

    def append[B >: A ](bst: Tree[B])(implicit ev: B => Ordered[B]): Tree[B]

    def ++[B >: A ](bst: Tree[B])(implicit ev: B => Ordered[B]): Tree[B]

    def preOrder[B](z: B)(f: (A, B) => B): B
  }

  case class Node[A](elem: A, left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def insert[B >: A](newElem: B)(implicit ev: B => Ordered[B]): Tree[B] =
      if (newElem < elem)
        Node(elem, left.insert(newElem), right)
      else if (newElem > elem)
        Node(elem, left, right.insert(newElem))
      else this

    override def append[B >: A](bst: Tree[B])(implicit ev: B => Ordered[B]): Tree[B] =
      bst.preOrder[Tree[B]](this)((e, acc) => acc.insert(e))

    override def preOrder[B](z: B)(f: (A, B) => B): B =
      right.preOrder(left.preOrder(f(elem, z))(f))(f)

    override def ++[B >: A](bst: Tree[B])(implicit ev: (B) => Ordered[B]): Tree[B] = append(bst)
  }

  case object Leaf extends Tree[Nothing] {
    override def insert[B](elem: B)(implicit ev: B => Ordered[B]): Tree[B] = Tree(elem)

    override def append[B](bst: Tree[B])(implicit ev: B => Ordered[B]): Tree[B] = bst

    override def preOrder[B](z: B)(f: (Nothing, B) => B): B = z

    override def ++[B](bst: Tree[B])(implicit ev: (B) => Ordered[B]): Tree[B] = append(bst)
  }


  object Tree {
    def apply(): Tree[Nothing] = Leaf

    def apply[A](elem: A, elems: A*)(implicit  ev: A => Ordered[A]): Tree[A] = {
      def recurse(elems: List[A], bst: Tree[A]): Tree[A] =
        if (elems.isEmpty) bst
        else recurse(elems.tail, bst.insert(elems.head))

      recurse(elems.toList, Node(elem, Leaf, Leaf))
    }
  }
}


object TreeApp extends App {

  import TreeImpl._
  val tree1 = Node(10, Leaf, Leaf)
  val tree2 = Node(5, Leaf, Leaf)


  println(tree1.append(tree2))
}


object FoldableExamples2 extends App {

  import TreeImpl._
  val tree: Tree[Int] = Node(1, Node(3, Leaf, Leaf), Node(5, Leaf, Leaf))

  implicit def foldableTree = new Foldable[Tree] {
    override def foldMap[A, B](fa: Tree[A])(f: (A) => B)(implicit F: Monoid[B]): B = fa match {
      case Leaf => F.zero
      case Node(elem, l, r) => {
        val lval = foldMap(l)(f)
        val rval = foldMap(r)(f)
        F.append(F.append(f(elem), lval), rval)
      }
    }

    override def foldRight[A, B](fa: Tree[A], z: => B)(f: (A, => B) => B): B =
      fa match {
        case Leaf => z
        case Node(elem, l, r) => f(elem, foldRight(l, foldRight(r, z)(f))(f))
      }
  }

  implicit def monoidTree[A](implicit ev: A => Ordered[A]) = new Monoid[Tree[A]] {
    override def zero: Tree[A] = Leaf

    override def append(f1: Tree[A], f2: => Tree[A]): Tree[A] = f1.append(f2)
  }

  implicit def functorTree = new Functor[Tree]{
    override def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
      case Leaf => Leaf
      case Node(e, l, r) => Node(f(e), map(l)(f), map(r)(f))
    }
  }

  implicit def applicativeTree = new Applicative[Tree] {
    override def point[A](a: => A): Tree[A] = {
      Node(a, Leaf, Leaf)
    }

    override def ap[A, B](fa: => Tree[A])(fTree: => Tree[A => B]): Tree[B] = ???
  }


//  def sum[F[_] : Foldable, A: Monoid](fa: F[A]): A =
//    fa.foldMap()
//
//  sum(List(1, 2, 3)) shouldEqual 6
//  sum(tree) shouldEqual 9
//  sum(List(1.some, 2.some, 3.some)) shouldEqual 6.some
//
//
//  def toList[F[_] : Foldable, A: Monoid](fa: F[A]): List[A] =
//    fa.foldMap(List(_))
//
//
//  // Generic Implementation of toList
//  toList(List(1, 2, 3)) shouldEqual List(1, 2, 3)
//  toList(List(1.some, 2.some, 3.some)) shouldEqual List(1.some, 2.some, 3.some)
//  toList(tree) shouldEqual List(1, 3, 5)


  def unit[F[_] : Applicative, A](a: A): F[A] = Applicative[F].pure(a)

  unit[Option, Int](19) shouldEqual 19.some


  // Generic implementation of Filter
  def filter[F[_] : Foldable : Applicative, A: Monoid](fa: F[A])(f: A => Boolean)(implicit ev: Monoid[F[A]]): F[A] =
    fa.foldMap { a =>
      if (f(a))
        unit[F, A](a)
      else
        Monoid[F[A]].zero
    }

  filter(List(1, 2, 3))(_ % 2 == 0) should contain theSameElementsAs List(2)
  filter(List(1.some, 2.some, 3.some))(xOpt => xOpt.fold(false)(_ % 2 == 0)) should contain theSameElementsAs List(2.some)

  val tree2: Tree[Int] = Tree(20,10,100)
  val result  = filter(tree2)(i => i % 2 == 0)

  println(tree2)
  println(result)


  val tree3: Tree[Int] = Tree(20,15, 10, 25, 100)
  val result3  = filter(tree2)(i => i % 2 == 0)

  println(tree3)
  println(result3)

//  def sum(x : Int)(y : Int) = x + y
//
//  val sum1: Tree[(Int) => Int] = Tree(20,15, 10, 25, 100) <*> Tree(sum)
//  val resultApp = Tree(20,15, 10, 25, 100) <*> sum1
//
//  println(resultApp)

}