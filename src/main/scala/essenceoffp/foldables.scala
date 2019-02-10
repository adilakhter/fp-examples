package essenceoffp


object foldablesExample01 extends App {

  import scalaz._


  trait Foldable[F[_]] {
    def foldMap[A, B](fa: F[A])(f: A ⇒ B)(implicit F: Monoid[B]): B
  }
}


object foldablesExample02 extends App {

  sealed trait Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]



  val binaryTree: Tree[String] = Node(Leaf("a"), Node(Leaf("b"), Leaf("c")))

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  implicit def foldableTree: Foldable[Tree] = new Foldable[Tree] {
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

  def reduce[F[_], A](fa: F[A]) (implicit F: Foldable[F],  m: Monoid[A]): A =
    fa.foldMap(identity) // fa.fold

  reduce(List("a","b","c")) shouldEqual "abc"

  reduce(List(Option(1), Option(2), Option(3))) shouldEqual Option(6)

  reduce(binaryTree) shouldEqual "abc"

  case class Product(productId: String, description: String, price: Int)

  val products = List(Product("1", "Nike", 1), Product("2", "Addidas", 2))

  val  average = products.foldMap(_.price) / products.foldMap(_ ⇒ 1)

  average |> println
}




object foldablesExample03 extends App {

  sealed trait Tree[A]
  case class Leaf[A](a: A) extends Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]



  val binaryTree: Tree[String] = Node(Leaf("a"), Node(Leaf("b"), Leaf("c")))

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

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

  def toList[F[_] : Foldable, A: Monoid](fa: F[A]): List[A] =
    fa.foldMap(List(_))


  toList(List(1, 2, 3)) shouldEqual List(1, 2, 3)
  toList(List(1.some, 2.some, 3.some)) shouldEqual List(1.some, 2.some, 3.some)
  toList(List(1.some, 2.some, 3.some)) shouldEqual List(1.some, 2.some, 3.some)


  def toList2[F[_] : Foldable, A, B](fa: F[A])(f: A ⇒ B): List[B] =
    fa.foldMap{a ⇒ List(f(a))}

}

object foldablesExample04 extends  App {

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  def str[A: Transformer, F[_] : Functor](fa: F[A]): F[String] =
    fa.map(Transformer[A].str(_))


  trait Transformer[A] {
    def str(a: A): String
  }

  object Transformer {
    @inline
    def apply[F](implicit F: Transformer[F]): Transformer[F] = F
  }

  case class Product(productId: String, description: String, price: Double)

  implicit val productTransformer = new Transformer[Product] {
    def str(a: Product): String = s"[Product]: ${a.productId}, ${a.description}, ${a.price}"
  }

  val aProduct = Product("123123", "Nike", 10.0)

  str(List(aProduct)) should contain theSameElementsAs List("[Product]: 123123, Nike")
  str(aProduct.some) shouldEqual Some("[Product]: 123123, Nike")

}