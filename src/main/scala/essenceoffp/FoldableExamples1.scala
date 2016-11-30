package essenceoffp

import scalaz._
import Scalaz._
import org.scalatest._
import Matchers._


object FoldableExamples1 extends App {

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

  def sum[F[_] : Foldable, A: Monoid](fa: F[A]): A =
    fa.foldMap()

  implicit def foldableTree = new Foldable[Tree] {
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


  implicit def monoidTree[T: Monoid] = new Monoid[Tree[T]] {
    override def zero: Tree[T] = Leaf(Monoid[T].zero)

    override def append(f1: Tree[T], f2: => Tree[T]): Tree[T] = Node(f1, f2)
  }

  sum(List(1, 2, 3)) shouldEqual 6
  sum(Node(Leaf(1.some), Node(Leaf(3.some), Leaf(5.some))).asInstanceOf[Tree[Option[Int]]]) shouldEqual 9.some
  sum(List(1.some, 2.some, 3.some)) shouldEqual 6.some


  def toList[F[_] : Foldable, A: Monoid](fa: F[A]): List[A] =
    fa.foldMap(List(_))


  toList(List(1, 2, 3)) shouldEqual List(1, 2, 3)
  toList(List(1.some, 2.some, 3.some)) shouldEqual List(1.some, 2.some, 3.some)
  toList(tree) shouldEqual List(1, 3, 5)


  def unit[F[_] : Applicative, A](a: A): F[A] = Applicative[F].pure(a)

  unit[Option, Int](19) shouldEqual 19.some


  def filter[F[_] : Foldable : Applicative, A: Monoid](fa: F[A])(f: A => Boolean)(implicit ev: Monoid[F[A]]): F[A] =
    fa.foldMap { a =>
      if (f(a))
        unit[F, A](a)
      else
        Monoid[F[A]].zero
    }

  filter(List(1, 2, 3))(_ % 2 == 0) should contain theSameElementsAs List(2)
  filter(List(1.some, 2.some, 3.some))(xOpt => xOpt.fold(false)(_ % 2 == 0)) should contain theSameElementsAs List(2.some)

  val result = filter(tree)(_ % 2 == 0)

  println(result)


  def sum(x: Int)(y: Int) = x + y

  val tree2: Tree[Int] = Node(Leaf(1), Node(Leaf(3), Leaf(5)))
  val sum1: Tree[(Int) => Int] = tree <*> Leaf(sum)
  val resultApp: Tree[Int] = tree2 <*> sum1

  println(tree)
  println(resultApp)


}


object FunctorTestApp extends App{

  def str[A:Transformer, F[_]: Functor](fa: F[A]): F[String] =
    fa.map(Transformer[A].str(_))


  trait Transformer[A] {
    def str(a: A): String
  }

  object Transformer{
      @inline def apply[F](implicit F: Transformer[F]): Transformer[F] = F
  }

  case class Product(productId: String, description: String)
  implicit val productTransformer = new Transformer[Product] {
    def str(a: Product): String = s"[Product]: ${a.productId}, ${a.description}"
  }


  val aProduct = Product("123123", "Nike")

  str(List(aProduct)) should contain theSameElementsAs List("[Product]: 123123, Nike")
  str(aProduct.some) shouldEqual Some("[Product]: 123123, Nike")
}