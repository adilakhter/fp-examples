package io.brabant.examples

import org.scalatest._
import Matchers._

import scala.concurrent.Future

trait Transformer[A] {
  def toString(a: A): String
}

case class Product(productId: String, description: String)
object Product {
  implicit val productTransformer = new Transformer[Product] {
    def toString(a: Product): String = s"[Product]: ${a.productId}, ${a.description}"
  }
}

object ProductApp extends App {
  val aProduct = Product("123123", "Nike")
  val productTransformer = implicitly[Transformer[Product]]

  productTransformer.toString(aProduct) shouldEqual """[Product]: 123123, Nike"""
}



object ProductApplicative extends App {

  import scalaz._, Scalaz._

  val aProduct = Product("123123", "Nike")

  def itemitemRecommendations(aProduct: Product): Option[List[Product]] = ???
  def visualRecommendations(aProduct: Product): Option[List[Product]] = ???

  def getAllRecommendations(aProduct: Product): Option[List[Product]] = (itemitemRecommendations(aProduct) |@| visualRecommendations(aProduct)) {_ ++ _}


  val allResult: Option[List[Product]] = (itemitemRecommendations(aProduct) |@| visualRecommendations(aProduct)) {_ ++ _}

  def product(id: String): Option[Product] = ???




  val result = for {
    p <- product("123123")
    recs <- getAllRecommendations(p)
  } yield recs




  throw new Error
//  def itemitemRecommendations2(aProduct: Product): Task[List[Product]] = ???
//  def visualRecommendations2(aProduct: Product): Task[List[Product]] = ???
//
//
//  val allResult2: Task[List[Product]] = (itemitemRecommendations2(aProduct) |@| visualRecommendations2(aProduct)) {_ ++ _}
}