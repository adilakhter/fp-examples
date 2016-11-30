package tlp

import org.scalatest.Matchers._

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

case class Place[T](a: T)


object example1 extends App {

  val k = Place(1)
  val v = Place(2)
  val entry = HEntry2(k, v)

  val hMap = HMap[Place, Place](Map(entry.key -> entry))

  // Place[T]
  hMap.get(k).map(_.value.a) shouldEqual Some(2)
  hMap.get2(k).map(_.a + 5) shouldEqual Some(4)
}