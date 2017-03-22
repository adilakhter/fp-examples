object writerMonadExamples01 extends App {

}


object listMonadTransformer01 extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Monad[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

    def pure[A](x: A): F[A]
  }

  object Monad {

    implicit object ListMonad extends Monad[List] {
      def map[A, B](fa: List[A])(f: A => B) = fa map f

      def flatMap[A, B](fa: List[A])(f: A => List[B]) = fa flatMap f

      def pure[A](x: A) = x :: Nil
    }

    implicit object OptionMonad extends Monad[Option] {
      def map[A, B](fa: Option[A])(f: A => B) = fa map f

      def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa flatMap f

      def pure[A](x: A) = Some(x)
    }

    def apply[F[_] : Monad]: Monad[F] = implicitly[Monad[F]]
  }
}
