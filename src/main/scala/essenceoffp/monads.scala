package essenceoffp


object monadsExample01 extends App {
  import org.scalatest.Matchers._

  trait Functor[F[_]] {
    def map[A, B](f : A => B): F[A] => F[B]
  }


  trait Applicative[F[_]] extends Functor[F] {

    def point[A](a: => A): F[A]

    def app[A, B](fa: ⇒ F[A])(f: F[A => B]): F[B]

  }


  trait Monad[F[_]] extends Applicative[F] {

    def bind[A, B](fa: F[A]) (afb: A ⇒ F[B]): F[B] // ← aka. flatMap

  }
}



object monadExample_IdentityMonad extends App {


}