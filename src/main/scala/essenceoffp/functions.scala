package essenceoffp

object functions1 {

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._


  val f: String => Int = s => s.length


  val x = 1 // ← value

  val add = (x: Int) => x + 1 // ← Functions are also values

  val addTwoInts: Int => (Int => Int) = x => y => x + y

  val addWithOne: Int => Int = addTwoInts (1)





}



  //                 ⇡----------⇡
  //             Higher-order Function

object functions2 {

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  val f: String => Int = str => str.length
  val g: Int => Int = i => i * i

  val h = f >>> g // >>> == andThen


  h("foo") shouldEqual 9
}

object functions3 {
  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  val f: String => Int = str => str.length
  val g: Int => Int = i => i * i

  val h: ((String, Int)) => (Int, Int) = f *** g // ← split combinator

  h(("foo", 10)) shouldEqual ((3, 100))
}


object functions4  extends App{
  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._


  //normalizes string
  val f: String => String = _.replaceAll("ß", "ss").replaceAll("[^a-zA-Z0-9-]+", "-")

  //checks if a string is a palindrome
  val g: String => Boolean = s => s == s.reverse

  // compose f and g with `andThen`
  val h: String => Boolean = f >>> g // >>> == andThen

  h("wow") shouldEqual true

}


object functions5  extends App{
  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._


  val f: String => String = // ← normalizes string
    _.replaceAll("ß", "ss").replaceAll("[^a-zA-Z0-9-]+", "-")

  val g: String => Boolean =
    s => s == s.reverse // ← checks if `s` is palindrome

  val h: ((String, String)) => (String, Boolean)
    = f *** g // ← split combinator

  h(("ma;am", "wow")) should be (("ma-am",true))

}


object functions6  extends App{
  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._


  val f: String => String = // ← normalizes string
    _.replaceAll("ß", "ss").replaceAll("[^a-zA-Z0-9-]+", "-")

  val g: String => Boolean =
    s => s == s.reverse // ← checks if `s` is palindrome

  val h: (String) => (String, Boolean) =
    f &&& g // ← combine combinator

  h("ma;am") should be (("ma-am",true))
}

