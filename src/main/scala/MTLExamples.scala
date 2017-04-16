//package io.xiaon.monadtransformers
//
//import cats.Monad
//import cats.data.Xor
//
//object UserLookup {
//
//  case class User(name: String, id: Long)
//
//  def lookupUserName(id: Long): Xor[Error, Option[String]] =
//    for {
//      optUser ← lookupUser(id)
//    } yield {
//      for {
//        user ← optUser
//      } yield user.name
//    }
//
//
//  def lookupUser(id: Long): Xor[Error, Option[User]] = ???
//
//  //  def compose[M1[_]: Monad, M2[_]: Monad] = {
//  //    type Composed[A] = M1[M2[A]]
//  //    new Monad[Composed] {
//  //      def pure[A](a: A): Composed[A] =
//  //        a.pure[M2].pure[M2]
//  //
//  //      def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] = ???
//  //
//  //      def tailRecM[A, B](a: A)(f: (A) => Composed[Either[A, B]]): Composed[B] = ???
//  //    }
//  //  }
//
//
//  // Monad Transformer -> allow us to squash Monads together
//  // It creates a monad where we previously had two or more.
//  // With this transformed monad -> we can avoid nested calls to `flatMap`.
//}
//
//
//object MTLExamples extends App {
//
//  import cats.Monad
//  import cats.instances.list._ //
//  import cats.syntax.applicative._ // gets the pure syntax
//  import cats.data.OptionT
//
//  type ListOption[A] = OptionT[List, A]
//
//
//
//}
