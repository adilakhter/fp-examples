package essenceoffp

object applicativesExample01 extends App {

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  1.pure[List] shouldEqual List(1)

  "test".pure[Option] shouldEqual Some("test")

  ""


}
