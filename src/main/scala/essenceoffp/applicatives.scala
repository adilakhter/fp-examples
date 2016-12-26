package essenceoffp

import scalaz.syntax.Ops

object applicativesExample00 extends  App {
  import scalaz._
  import Scalaz._

  val x = 1.some
  val y = 2.some

  val add = (x: Int) ⇒ (y:Int) ⇒ x + y

  // Can we use Functor machinary to sum up these Options?


}




object applicativesExample01 extends App {

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  1.pure[List] shouldEqual List(1)

  "scala".pure[Option] shouldEqual Some("scala")

  val add2Ints: Int => Int => Int = x => y => x + y

  add2Ints.pure[Option] shouldEqual Some(add2Ints)
}


object applicativesExample02 extends App {

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  val add: Int ⇒ Int ⇒ Int => Int
  = x ⇒ y ⇒ z ⇒ x + y + z

  add.pure[Option] shouldEqual Some(add)

  // Option[(Int) => (Int) => (Int) => Int]
  val addOpt: Option[(Int) => (Int) => (Int) => Int] = add.pure[Option]
}


object applicativesExample04 extends App {

  import org.scalatest.Matchers._

  trait Functor[F[_]] {
    def map[A, B](f : A => B): F[A] => F[B]
  }


  trait Applicative[F[_]] extends Functor[F] {

    def app[A, B](fa: ⇒ F[A])(f: F[A => B]): F[B]

    def point[A](a: => A): F[A]
  }

  implicit val OptionApplicative = new Applicative[Option] {
    def point[A](a: => A): Option[A] =
      Some(a)

    def app[A, B](fa: => Option[A])(f: Option[(A) => B]): Option[B] =
    f match {
      case None ⇒ None
      case Some(f) ⇒ fa match {
        case None ⇒ None
        case Some(a) ⇒ Some(f(a))
      }
    }

    def map[A, B](f: (A) => B): (Option[A]) => Option[B] = ???

  }


  val xOpt = OptionApplicative.point(1)
  val yOpt = OptionApplicative.point(2)
  val zOpt = OptionApplicative.point(3)

  val add: Int ⇒ Int ⇒ Int => Int
  = x ⇒ y ⇒ z ⇒ x + y + z

  val addOpt = OptionApplicative.point(add)



  case class ApplicativeOps[F[_]: Applicative,A] (self: F[A]) {
    val  F = implicitly[Applicative[F]]

    def <*>[B](f: F[A => B]): F[B] = F.app(self)(f)
  }

  implicit def ToApplicativeOps[F[_]: Applicative, A](v: F[A]): ApplicativeOps[F, A] =
    ApplicativeOps(v)

  val result = zOpt <*> (yOpt <*> (xOpt <*> addOpt))

  result shouldEqual Some(6)






}


object applicativesExample03 extends App {

  import scalaz._
  import Scalaz._
  import org.scalatest.Matchers._

  val add: Int ⇒ Int ⇒ Int => Int
  = x ⇒ y ⇒ z ⇒ x + y + z

  add.pure[Option] shouldEqual Some(add)


  val xOpt = 1.some
  val yOpt = 2.some
  val zOpt = 3.some

  val result= xOpt.map(x ⇒ yOpt.map(y ⇒ x+y)).flatten

  val x = 1
  val y = 2
  val z = 3

  val result2 = add(x)(y)(z)

  val sumX: Option[Int => Int => Int] =  xOpt <*> add.pure[Option]
  val sumY: Option[Int => Int]        =  yOpt <*> sumX
  val sumZ: Option[Int]               =  zOpt <*> sumY

  val result3: Option[Int] =  sumZ


  sumZ shouldEqual (6.some)


  val result4: Option[Int] = (xOpt |@| yOpt |@| zOpt) { (x, y, z) => add(x)(y)(z) }
  result4 shouldEqual (6.some)


  val result5: Option[Int] = zOpt <*> (yOpt <*> (xOpt <*> add.some))


  result5 shouldEqual (6.some)
}

