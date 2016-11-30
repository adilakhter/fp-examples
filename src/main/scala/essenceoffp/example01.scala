package essenceoffp

import scala.io.StdIn
import scalaz.Monad
import scalaz._
import Scalaz._
import scala.util.Try

import org.scalatest._

object example01 extends App {

  case class Coordinate(longitude: Double, lat: Double)

  def readCoordinate(): Coordinate = {
    println("Latitude:")
    val longitude = StdIn.readLine().toDouble
    println("Longitude:")
    val latitude = StdIn.readLine().toDouble
    Coordinate(longitude, latitude)
  }

  // 37.773972, -122.431297.


  final case class IO[A](unsafePerformIO: () => A)

  def readLine2: IO[String] = IO(() => readLine())

  def println2(s: String): IO[Unit] = IO(() => println(s))

  def io[A](computation: =>A): IO[A] = IO(() => computation)

  implicit val monadIO: Monad[IO] = new Monad[IO] {
    override def point[A](a: => A): IO[A] = IO(() => a)

    override def bind[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] =
      IO(() => f(fa.unsafePerformIO()).unsafePerformIO())
  }


  val ioMonad: IO[(String, String)] =
    for {
      _ ← println2("Latitude:")
      x ← readLine2
      _ ← println2("Longitude:")
      y ← readLine2
    } yield (x, y)


//  val result = ioMonad.unsafePerformIO()

 // println(result)




  sealed trait ToEither2[A, B] {
    def toEither: \/[A, B]
  }

  object ToEither2 {
    implicit def tryToEither[A](t: Try[A]): ToEither2[Throwable, A] =
      new ToEither2[Throwable, A] {
        override def toEither: Disjunction[Throwable, A] = t.map(\/-(_)).recover { case e => -\/(e) }.get
      }
  }

  val readCoordinate2: IO[Option[Coordinate]] =
    for {
      _ ← println2("Latitude:")
      x ← readLine2
      _ ← println2("Longitude:")
      y ← readLine2
      latitude  <- io(Try(x.toDouble).toOption)
      longitude <- io(Try(y.toDouble).toOption)
      c <- io((latitude |@| longitude){Coordinate})
    } yield c

  readCoordinate2.unsafePerformIO() |> println
}

object exampl02app extends App {
  case class Id[A](value: A)

  implicit  val identityMonad = new Monad[Id] {
    def point[A](a: => A): Id[A] = Id(a)
    def bind[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] = f(fa.value)
  }


  val idMonad = implicitly[Monad[Id]]

  val x: Id[Int] = Id(1)
  val y: Id[Boolean] = Id(1).flatMap(i => Id(i%2 == 0))

  println(y)
}
