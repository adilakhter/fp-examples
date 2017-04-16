package lambdaconf.patterns


import scalaz._
import Scalaz._
import scala.io.StdIn
import scala.util.Try
import scalaz.Alpha.G

object exercise1 {
  import ToEither2._
  def readRowCol(): (Int, Int) = {
    println("Please enter a row:")
    val row = StdIn.readInt()
    println("Please enter a column:")
    val col = StdIn.readInt()
    (row, col)
  }

  def readRowCol2()  = {
    println("Please enter a row:")
    val row = StdIn.readInt()
    println("Please enter a column:")
    val col = StdIn.readInt()

    for {
      a <- Try(row.toInt).toEither
      b <- Try(col.toInt).toEither
    } yield (a, b)
  }

  sealed trait ToEither2[A, B] {
    def toEither: \/[A, B]
  }

  object ToEither2 {
    implicit def tryToEither[A](t: Try[A]): ToEither2[Throwable, A] =
      new ToEither2[Throwable, A] {
        override def toEither: Disjunction[Throwable, A] = t.map(\/-(_)).recover { case e => -\/(e) }.get
      }
  }

}


object exercise2 {

  sealed trait Node
  case object Root extends Node
  final case class Child(parent: Node, name: String) extends Node



  implicit val NodeMonoid: Monoid[Node] = new Monoid[Node] {
    override def zero: Node = Root

    override def append(f1: Node, f2: => Node): Node = (f1, f2) match {
      case (Root, x) => x
      case (x, Root) => x
      case (x, Child(p2, n2)) => Child(append(x , p2), n2)
    }
  }
}

object exercise3 {
  final case class IO[A](unsafePerformIO: () => A)

  def readLine2: IO[String] = IO( () => readLine())
  def println2(s: String): IO[Unit] = IO(() => println(s))

  implicit val monadIO: Monad[IO] = new Monad[IO]{
    override def point[A](a: => A): IO[A] = IO(() => a)

    override def bind[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] =
      IO(() => f(fa.unsafePerformIO()).unsafePerformIO())
  }
}


object exercise3App extends App {

  import exercise3._
  import ToEither2._


  sealed trait ToEither2[A, B] {
    def toEither: \/[A, B]
  }

  object ToEither2 {
    implicit def tryToEither[A](t: Try[A]): ToEither2[Throwable, A] =
      new ToEither2[Throwable, A] {
        override def toEither: Disjunction[Throwable, A] = t.map(\/-(_)).recover { case e => -\/(e) }.get
      }
  }

  val ioResult =
    for {
    _ ← println2("Please enter a row:")
    x ← readLine2
    _ ← println2("please enter a column")
    y ← readLine2
    a <- IO(() ⇒ Try(x.toInt).toEither)
    b <- IO(() ⇒ Try(y.toInt).toEither)
    } yield (a, b)


  val result = ioResult.unsafePerformIO()
  val result2 = List(result._1, result._2)
  val test = result2.sequenceU

  /**
   * Please enter a row: 1
   * please enter a column: 2
   */
  test  // ("1","2")






}
object exercise4 {
  def filter[A](f: A => Boolean, l: List[A]): List[A] = {
    val foldable = Foldable[List]
    foldable.foldMap(l)(a => if(f(a)) List(a) else List())
    //def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): Unit = ???
    // case class Sum(value: Int)
    //Foldable[List[Int]].foldMap(List(1,2,3))()

  }
}

object exercise5 {
  trait List[A] { self =>
    def fold[Z](nil: => Z, cons: (Z, A) => Z): Z

    final def :: (next: A): List[A] = new List[A] {
      def fold[Z](nil: => Z, cons: (Z, A) => Z): Z = {
        cons(self.fold(nil, cons), next)
      }
    }
  }
  object List {
    def empty[A]: List[A] = new List[A] {
      def fold[Z](nil: => Z, cons: (Z, A) => Z): Z = nil
    }
  }

  implicit val ListTraverse: Traverse[List] = ???
}
