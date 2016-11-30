package lambdaconf.typeclasses

import matryoshka._
import monocle._

import scalaz._
import Scalaz._
import scala.util.Try

object exercise1 {
  sealed trait PathLike[A] {
    def concat(first: A, second: A): A

    def root: A // root does not contain any information


  }


  object PathLike {
    def apply[A: PathLike]: PathLike[A] = implicitly[PathLike[A]]
  }
}

object exercise2 {
  import exercise2._

  // concat (concat(a, b), c) == concat(a, concat(b,c))
  // concat (root, a ) == a
  // concat (a, root)  == a
}

object exercise3 {
  import exercise1._
  import exercise2._

  sealed trait Node
  final case object Root extends Node
  final case class Child(parent: Node, name: String) extends Node

  implicit val NodePathLike: PathLike[Node] = new PathLike[Node] {
    // ???
    override def concat(first: Node, second: Node): Node = (first, second) match {
      case (Root, x) => x
      case (x, Root) => x
      case (x, Child(p2, n2)) => Child(concat(x , p2), n2)

    }

    override def root: Node = Root
  }
}

object exercise4 {
  import exercise1._
  import exercise2._
  import exercise3._

  implicit class PathLikeSyntax[A: PathLike](self: A) {
    def /(next: A): A = PathLike[A].concat(self, next)
  }

  def root[A: PathLike]: A = PathLike[A].root


  root[Node] / root[Node]
}


// Notes :
object MainAppTypeClasses extends  App {

  trait Debug[A] {
    def show(a: A): String

    def read(a: String): Either[String, A]

  }

  object Debug {
    def apply[A](implicit v: Debug[A]): Debug[A] = v

    implicit val IntDebug: Debug[Int] = new Debug[Int] {
      override def show(a: Int): String = a.toString

      override def read(a: String): Either[String, Int] =
        Try(a.toInt).map(Right(_)).getOrElse(Left("the values is ...."))
    }
  }


  println(Debug[Int].show(1))


  implicit class DShowSyntax[A: Debug](self: A) {
    def showIt: String = Debug[A].show(self)

    def printIt: Unit = println(Debug[A].show(self))
  }

  100.printIt

}