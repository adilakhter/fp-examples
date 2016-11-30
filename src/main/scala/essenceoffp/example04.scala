package essenceoffp

import scalaz.{Functor, Monoid}
import org.scalatest
import org.scalatest.Matchers._

/**
  * Created by adil on 11/11/16.
  */
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

  val sumX: Option[Int => Int => Int] =  xOpt <*> sum.some
  val sumY: Option[Int => Int]        =  yOpt <*> sumX
  val sumZ: Option[Int]               =  zOpt <*> sumY

  val result3: Option[Int] =  sumZ


  sumZ shouldEqual (6.some)


  val result4: Option[Int] = (xOpt |@| yOpt |@| zOpt) { (x, y, z) => sum(x)(y)(z) }
  result4 shouldEqual (6.some)


  val result5: Option[Int] = zOpt <*> (yOpt <*> (xOpt <*> sum.some))


  result5 shouldEqual (6.some)
}



object Example5 extends App {

  import scalaz._
  import Scalaz._

  trait HEntry2[K[_], V[_]] {
    // * -> *

    type A

    def key: K[this.A]

    def value: V[this.A]
  }

  object HEntry2 {
    def apply[T, K[_], V[_]](k: K[T], v: V[T]): HEntry2[K, V] =
      new HEntry2[K, V] {
        type A = T

        def key: K[this.A] = k

        def value: V[this.A] = v
      }
  }


  case class HMap[K[_], V[_]](data: Map[K[_], HEntry2[K, V]]) {
    def get(key: K[_]): Option[HEntry2[K, V]] = data.get(key)

    def get2[T](key: K[T]): Option[V[T]] = data.get(key).map(entry => entry.value.asInstanceOf[V[T]])

    def add(hEntry: HEntry2[K, V]) = data.+(hEntry.key -> hEntry)

    // K[HEntry2[K, V]#A]
  }


  case class Place[T] (a: T)

  val k = Place(1)
  val v = Place(2)


  val entry = HEntry2(k, v)

  val hMap = HMap[Place, Place](Map(entry.key -> entry))

  val result = hMap.get(k).map(_.value.a) // Place[T]

  println(result)


  val result2 = hMap.get2(k).map(_.a + 5)

  println(result2)

}