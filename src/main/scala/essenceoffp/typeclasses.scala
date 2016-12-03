package essenceoffp

object typeclasses01 extends App{

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  trait Transformer[A] {
    def toString(a: A): String
  }

  case class Product(productId: String, description: String)

  object Product {
    implicit val productTransformer =
      new Transformer[Product] {
        def toString(a: Product): String
        = s"[Product]: ${a.productId}, ${a.description}"
      }
  }

  val aProduct = Product("123123", "Nike")
  val productTransformer = implicitly[Transformer[Product]]

  productTransformer.toString(aProduct) shouldEqual """[Product]: 123123, Nike"""

}
