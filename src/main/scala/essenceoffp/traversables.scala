package essenceoffp

import essenceoffp.FoldableExamples1.{Leaf, Node}
import essenceoffp.foldablesExample03.{Leaf, Node}


object traversablesExample01 extends App{
import scalaz._
  import Scalaz._

  trait Traverse[F[_]] extends Functor[F] with Foldable[F]{

    def traverse[G[_]: Applicative, A, B](f: A => G[B]): F[A] => G[F[B]]

    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]]

  }
}



object traversablesExample02 extends App {

  import scalaz._
  import Scalaz._

  import org.scalatest.Matchers

  sealed trait Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  val tree: Tree[Int] = Node(Leaf(1), Node(Leaf(3), Leaf(5)))

  implicit val TreeFunctor = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: (A) => B): Tree[B] = fa match {
      case Leaf(a: A) => Leaf(f(a))
      case Node(left, right) => Node(map(left)(f), map(right)(f))
    }
  }
  implicit val TreeApplicative = new Applicative[Tree] {
    override def point[A](a: => A): Tree[A] = Leaf(a)

    override def ap[A, B](fa: => Tree[A])(ft: => Tree[(A) => B]): Tree[B] = fa match {
      case Leaf(x) => ft.map(f => f(x))
      case Node(l, r) => Node(ap(l)(ft), ap(r)(ft))
    }
  }
  implicit val foldableTree = new Foldable[Tree] {
    override def foldMap[A, B](fa: Tree[A])(f: (A) => B)(implicit F: Monoid[B]): B = fa match {
      case Leaf(v) => f(v)
      case Node(l, r) => F.append(foldMap(l)(f), foldMap(r)(f))
    }

    override def foldRight[A, B](fa: Tree[A], z: => B)(f: (A, => B) => B): B =
      fa match {
        case Leaf(v) => f(v, z)
        case Node(l, r) => foldRight(l, foldRight(r, z)(f))(f)
      }
  }


  def leaf[A] = (n: A) => Leaf(n): Tree[A]
  def node[A] = (nl: Tree[A]) => (nr: Tree[A]) => Node(nl, nr): Tree[A]


  //  def traverseImpl[F[_], A, B](fa: Option[A])(f: A => F[B])(implicit F: Applicative[F]) =
  //    fa map (a => F.map(f(a))(Some(_): Option[B])) getOrElse F.point(None)

  implicit val TreeTraverse = new Traverse[Tree] {
    def traverseImpl[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      fa match {
        case Leaf(a) ⇒ f(a) <*> G.point(leaf[B])
        case Node(l, r) ⇒ traverseImpl(r)(f) <*> (traverseImpl(l)(f) <*> G.point(node[B]))
      }
  }

  import org.scalatest.Matchers._

  val binaryTree: Tree[Option[Int]] =
    Node(Leaf(Some(1)), Node(Leaf(Some(2)), Leaf(Some(3))))
  val binaryTree2: Tree[Option[Int]] =
    Node(Leaf(Some(1)), Node(Leaf(Some(2)), Leaf(None)))


  binaryTree.sequence[Option, Int] shouldEqual Some(Node(Leaf(1),Node(Leaf(2), Leaf(3))))
  
  binaryTree2.sequence[Option, Int] shouldEqual None
}