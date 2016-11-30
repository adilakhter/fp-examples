package essenceoffp

import org.scalatest.Matchers._

object funcCompose1 extends App{

  val f: String => Int = str => str.length
  val g: Int => Int = i => i * i


  import scalaz.Scalaz._


  val h = f >>> g // >>> == andThen


  h("foo") shouldEqual 9
}

object funcCompose3 extends App{

  val f: String => Int = str => str.length
  val g: Int => Int = i => i * i


  import scalaz.Scalaz._


  val h: ((String, Int)) => (Int, Int) = f *** g


  h(("foo", 10)) shouldEqual ((3, 100))
}

object funcCompose2 extends App{

  val f: String => Int = str => str.length
  val g: String => Boolean = str => str == str.reverse

  import scalaz.Scalaz._

  val h: (String) => (Int, Boolean) = f &&& g


  h("foo") shouldEqual ((3, false))
  h("madam") shouldEqual ((5, true))
}


object functionCurryingExample extends App {

  val add2: Int => Int => Int = x => y => x + y

  val add: Int => Int = add2(1)

  add(2) shouldEqual 3

  //val sum1: Int => Int = x => x + 1

}