package lambdaconf.types

import matryoshka._
import monocle._
import scalaz._
import Scalaz._

object exercise1 {
  final case class CheckersBoard(board: List[List[CheckerPiece]])

  sealed trait CheckerPiece
  case object White extends CheckerPiece
  case object Black extends CheckerPiece

  sealed trait SquareState
  case object Empty extends SquareState
  case object White2 extends SquareState
  case object Black2 extends SquareState

}

object exercise2 {
  final case class Box[A](a: A)
}

object exercise3 {
  // 1. scala.collection.List |  * => *
  // 2. F[_, _] | [*, *] => *
  // 3. Option | * => *
  // 4. Int | *
  // 5. T[_[_], _] | [(* => *),  *] => *
}

object exercise4 {
  trait FileSystem {
    type File
    def ls: List[File]
  }
  val fs: FileSystem = ???

//  lazy val myFiles:List[fs#File] = fs.ls
}



object exercise5 {
  sealed trait Example[F[_]] {
    def value: F[String]
  }


  new Example[Either[?, Int]] { // <-- ???
    def value: Either[String, Int] = Right(2)
  }

  new Example[({type L[A] = Either[A, Int]})#L] { // <-- ???
  def value: Either[String, Int] = Right(2)
  }

  type L[A] = Either[A, Int]

  new Example[L] { // <-- ???
    def value: Either[String, Int] = Right(2)
  }
}

// Fairly strong assertion.
// with `Product` and `Sum` types can model any structure.
sealed trait DayOfWeek
case object Sunday extends DayOfWeek


// modeling a Tree
trait Tree[A]
case class Leaf[A] (a: A) extends Tree[A]
case class Branch[A] (l: Tree[A], r: Tree[A]) extends Tree[A]


//  Tree is a