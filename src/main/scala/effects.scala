package lambdaconf.effects

import lambdaconf.effects.exercise3.State.Person
import matryoshka._
import monocle._

import scalaz._
import Scalaz._


object exercise1 {
  final case class IO[A](/* ??? */)

  implicit val IOMonad: Monad[IO] = ???
}

object exercise2 {
  import exercise1._

  sealed trait ConsoleF[A]

  val program: Free[ConsoleF, Unit] = ???
}

object exercise3 {

  // Feed a state
  // computation of A that in
  final case class State[S, A](run: S => (A, S)) {
    def evalState(s: S): A = run(s)._1
  }

  object State {
    def get[S]: State[S, S]  = State(s => (s,s))

    def set[S](s: S) : State[S, S] = State(_ => (s, s))

    def modify[S](f: S => S) : State[S, S] = State(s => (f(s),s ))

    case class Person(name: String, age: Int)

    type StateP[A] = State[Person, A]

    val program: StateP[Unit]=
      for {
      p <- State.get[Person]
      _ <- State.set[Person](p.copy(age = p.age+1))
    } yield()
  }




  implicit def StateMonad[S]: Monad[State[S, ?]] = new Monad[State[S, ?]]{

    override def point[A](a: => A): State[S, A] =
      State(s => (a,s))

    override def bind[A, B](fa: State[S, A])(f: (A) => State[S, B]): State[S, B] = {
      State { s =>
        val (a, s1) = fa.run(s)
        f(a).run(s1)
      }
    }

  }
}

object exercise4 {
  import exercise3._

  def sort[A: Order](list: List[A]): List[A] =
    (??? : State[List[A], List[A]]).evalState(list)
}



object TestMonadT{
case class StateT[S, F[_], A](run: S => F[(S,A)])

implicit def MonadStateT[S, F[_]: Monad] = ??? //new MonadStateT[S]

// type Program[A] = StateT[Person, Reader[Config, ?], A]



}
