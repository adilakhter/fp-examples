package eg

import cats.data.EitherT
import cats.implicits._
import cats.{Id, Monad}

import scala.concurrent.Future

object TaglessFinalDemo extends App {

  sealed trait Msg
  final case class CreateIndexResponse(indexName: String) extends Msg
  final case class DeleteIndexResponse(indexName: String) extends Msg

  trait IndexDsl[F[_]] {
    def createIndex(name: String): F[Either[String, CreateIndexResponse]]
    def deleteIndex(name: String): F[Either[String, DeleteIndexResponse]]
  }

  object IndexDsl {
    implicit def asyncDsl: IndexDsl[Future] = new IndexDsl[Future] {
      override def createIndex(
                                name: String): Future[Either[String, CreateIndexResponse]] = ???
      override def deleteIndex(
                                name: String): Future[Either[String, DeleteIndexResponse]] = ???
    }
    implicit def syncDsl: IndexDsl[Id] = new IndexDsl[Id] {
      override def createIndex(name: String): Id[Either[String, CreateIndexResponse]] = CreateIndexResponse(name).asRight[String]
      override def deleteIndex(name: String): Id[Either[String, DeleteIndexResponse]] = DeleteIndexResponse(name).asRight[String]
    }
  }

  def recreateIndex[F[_]: Monad](name: String)(
      implicit interpreter: IndexDsl[F])
    : F[Either[String, CreateIndexResponse]] = {
    val newIndex: EitherT[F, String, CreateIndexResponse] = for {
      _ <- EitherT(interpreter.deleteIndex(name))
      created <- EitherT(interpreter.createIndex(name))
    } yield created

    newIndex.value
  }

  import IndexDsl._
  val response = recreateIndex[Id]("test_index")
  println(response.fold(identity, _.indexName.toString))

}
