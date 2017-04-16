package misc

import cats.free.Free
import cats.{Id, ~>}


object freeExamples {

  // Example of Free monad in
  // Cats

//  def pure[A](a: A): M[A]
//
//  def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B]
//

  trait Container[T]
  case class StringContainer(value: String) extends Container[String]
  val lifted: Free[Container, String] =
      Free.liftF[Container, String](StringContainer("foo"))

  case class Position(x: Double, y: Double, heading: Degree)
  case class Degree(private val d: Int) {
    val value = d % 360
  }

  object Logo {
    sealed trait Instruction[A]
    case class Forward(position: Position, length: Int) extends Instruction[Position]
    case class Backward(position: Position, length: Int) extends Instruction[Position]
    case class RotateLeft(position: Position, degree: Degree) extends Instruction[Position]
    case class RotateRight(position: Position, degree: Degree) extends Instruction[Position]
    case class ShowPosition(position: Position) extends Instruction[Unit]


  }
  object DSL {
    import Logo._
    def forward(pos: Position, l: Int): Free[Instruction, Position] =
      Free.liftF(Forward(pos, l))
    def backward(pos: Position, l: Int): Free[Instruction, Position] =
      Free.liftF(Backward(pos, l))
    def left(pos: Position, degree: Degree): Free[Instruction, Position] =
      Free.liftF(RotateLeft(pos, degree))
    def right(pos: Position, degree: Degree): Free[Instruction, Position] =
      Free.liftF(RotateRight(pos, degree))
    def showPosition(pos: Position): Free[Instruction, Unit] =
      Free.liftF(ShowPosition(pos))
  }



  object Computations {
    def forward(pos: Position, l: Int): Position = pos.copy(
      x=pos.x + l*math.cos(pos.heading.value * math.Pi/180.0),
      y=pos.y + l*math.sin(pos.heading.value * math.Pi/180.0))

    def backward(pos: Position, l: Int): Position = pos.copy(
      x=pos.x - l*math.cos(pos.heading.value * math.Pi/180.0),
      y=pos.y - l*math.sin(pos.heading.value * math.Pi/180.0))

    def left(pos: Position, d: Degree): Position = pos.copy(
      heading=Degree(pos.heading.value + d.value))

    def right(pos: Position, d: Degree): Position = pos.copy(
      heading=Degree(pos.heading.value - d.value))
  }

  import cats.{Id,~>}
  import Logo._

  object InterpreterId extends (Instruction ~> Id) {
    import Computations._
    override def apply[A](fa: Instruction[A]): Id[A] = fa match {
      case Forward(p, length) => forward(p, length)
      case Backward(p, length) => backward(p, length)
      case RotateLeft(p, degree) => left(p, degree)
      case RotateRight(p, degree) => right(p, degree)
      case ShowPosition(p) => println(s"showing position $p")
    }
  }

}


object InterpreterIdApp extends App {
  import freeExamples._
  import freeExamples.Logo._
  import freeExamples.DSL._

  val program: (Position ⇒ Free[Instruction, Position]) = {
    start: Position ⇒
      for {
        p1 ← forward(start, 10)
        p2 ← right(p1, Degree(90))
        p3 ← forward(p2, 10)
      } yield p3
  }

  val startPosition = Position(0.0, 0.0, Degree(0))
  val result: Id[Position] = program(startPosition).foldMap(InterpreterId)

  println(s"result = $result")
}

