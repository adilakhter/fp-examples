package misc

import scalaz._
import Scalaz._
import scalaz.concurrent._
import scala.util.{Failure, Success, Try}

object UnsafeMethod {
  def divideBy(x: Int, y: Int) : Int  =
    x/y
}


object MonadErrorExamples extends App {

  import UnsafeMethod._

  def computeSomeStuff[F[_]](i: Int)(implicit merr: MonadError[F, Throwable]): F[Int] =
    Try(divideBy(10, i)) match {
      case Success(x) ⇒ x.pure[F]
      case Failure(e) ⇒ merr.raiseError(e)
    }

  type Error[A] = Throwable \/ A

  println(computeSomeStuff[Error](0))
  println(computeSomeStuff[Task](0).attempt.unsafePerformSync)
}



