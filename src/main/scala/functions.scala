package lambdaconf.functions
import scalaz._
import Scalaz._

object exercise1 {
  // Domain: {Vegetables, Fruits, Meat, Dairy, Eggs}
  // Codomain: {Love, Like, Neutral, Dislike, Hate}
}

object exercise2 {
  val compareStrings: (Char => Char) => (String, String) => Boolean =
    fn => (fst, snd) => {
      fst.zipAll(snd, "", "").toSeq.exists { case (x1, x2) => x1 != x2 }
    }
}

object exercise3 {
  type Parser[A] = String => Either[String, (String, A)]

  def or[A](left: Parser[A], right: Parser[A]): Parser[A] =
    (str) => {
      val first = left(str)
      first.fold(l => right(str), r => first)
    }
  
  def seq[A, B] (fist: Parser[A], right: Parser[B]): Parser[(A, B)] = ???
}

object exercise4 {
  def snd[A, B](v: (A, B)): B = v._2

  trait snd2 {
    def apply[A, B] (v: (A, B)):B = v._2
  }

  object oneOf{
    def apply[A](v: (A, A)): A = v._1
  }
}