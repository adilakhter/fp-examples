package essenceoffp

import scalaz.{Functor, Monoid}
import org.scalatest
import org.scalatest.Matchers._

object example04 extends  App{

  implicit val intMonoid = new Monoid[Int] {
    val zero = 0

    def append(f1: Int, f2: => Int): Int = f1 + f2
  }

  Monoid[Int].append(4, 6) shouldEqual 10
}



object exampleApplicative02 extends App {

  import scalaz._
  import Scalaz._

  val sum: (Int ⇒ Int ⇒ Int =>Int) = (x:Int) ⇒ (y: Int) => (z: Int) ⇒ x + y + z

  val xOpt = 1.some
  val yOpt = 2.some
  val zOpt = 3.some

  val result= xOpt.map(x ⇒ yOpt.map(y ⇒ x+y)).flatten

  val x = 1
  val y = 2
  val z = 3

  val result2 = sum(x)(y)(z)

  val sumX: Option[Int => Int => Int] =  xOpt <*> sum.pure[Option]
  val sumY: Option[Int => Int]        =  yOpt <*> sumX
  val sumZ: Option[Int]               =  zOpt <*> sumY

  val result3: Option[Int] =  sumZ


  sumZ shouldEqual (6.some)


  val result4: Option[Int] = (xOpt |@| yOpt |@| zOpt) { (x, y, z) => sum(x)(y)(z) }
  result4 shouldEqual (6.some)


  val result5: Option[Int] = zOpt <*> (yOpt <*> (xOpt <*> sum.some))


  result5 shouldEqual (6.some)
}


