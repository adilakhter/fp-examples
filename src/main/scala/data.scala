package lambdaconf.data

import matryoshka._
import monocle._
import scalaz.{Lens => _, _}

import Scalaz._

object exercise1 {
  // 1. All prime numbers  => codata
  // 2. A JSON document  => data
}

object exercise2 {
  sealed trait Boolean {
    def apply[A](ifTrue: => A, ifFalse: => A) : A
  }
  object Boolean {
    val True = new Boolean {
      def apply[A](ifTrue: => A, ifFalse: => A) : A = ifTrue
    }
    val False = new Boolean {
      def apply[A](ifTrue: => A, ifFalse: => A) : A = ifFalse
    }
  }
  import Boolean._
  True(ifTrue = 1, ifFalse = 2)

  sealed trait Either[A, B] {
    def apply[Z](left: A => Z, right: B => Z)
  }
  object Either {
    def left[A, B](v: A): Either[A, B] = new Either[A, B] {
      def apply[Z](left: A => Z, right: B => Z) = left(v)
    }
    def right[A, B](v: B): Either[A, B] = new Either[A, B] {
      def apply[Z](left: A => Z, right: B => Z) = right(v)
    }

    val result: Either[String, Int] = Either.left("...")

    val did = result(left = _ => false, right= _ => true)
  }

  // Cheat: type Option[A] = Either[Unit, A]
  sealed trait Option[A] {
    def apply[Z](some: A => Z, none : => Z)
  }
  object Option {
    def none[A]: Option[A] = new Option[A] {
      override def apply[Z](some: (A) => Z, none: => Z) = none
    }
    def some[A](v: A): Option[A] = new Option[A] {
      override def apply[Z](some: (A) => Z, none: => Z) = some(v)
    }
  }
}

object exercise3 {
  trait Natural { self =>
    def fold[Z](zero: => Z, succ: Z => Z): Z

    def succ: Natural = new Natural {
      def fold[Z](zero: => Z, succ: Z => Z): Z = succ(self.fold(zero, succ))
    }
    def + (that: Natural): Natural = new Natural {
      def fold[Z](zero: => Z, succ: Z => Z): Z = that.fold[Natural](self, _.succ).fold[Z](zero, succ)
    }
    def * (that: Natural): Natural = new Natural {
      def fold[Z](zero: => Z, succ: Z => Z): Z =
        that.fold[Natural](Natural.zero, _ + self).fold[Z](zero, succ)
    }
    def isZero: Boolean = fold[Boolean](true, _ => false)
    def toInt: Int = fold[Int](0, _ + 1)
    override def toString = toInt.toString
  }
  object Natural {
    val zero = new Natural { def fold[Z](zero: => Z, succ: Z => Z): Z = zero }
    def of(v: Int): Natural = if (v == 0) zero else of(v - 1).succ
  }

  trait Integer { self =>
    // ???
  }

  case class Integer0(pos: Natural, neg: Natural){
    def + (that: Integer0) = Integer0(pos + that.pos, neg + that.neg)

    def unary_- = Integer0(neg, pos)

      // def - (that: Integer0) = this + (-that)
  }
}

object exercise4 {
  sealed trait JSON
  final case object Null extends JSON
  final case class Array(value: List[JSON]) extends JSON
  final case class Object(value: List[(String, JSON)]) extends JSON
  final case class Number(value: String) extends JSON
  final case class Boolean(value: Boolean) extends JSON

  val _Null     : Prism[JSON, Unit] = ???
  val _Array    : Prism[JSON, List[JSON]] = ???
  val _Object   : Prism[JSON, List[(String, JSON)]] = ???
  val _Number   : Prism[JSON, String] = ???
  val _Boolean  : Prism[JSON, Boolean] = ???
}

object exercise5 {
  import exercise4._

  sealed trait ContactType
  case object Business extends ContactType
  case object Personal extends ContactType

  case class Person(name: String, age: Int, contactType: ContactType)

  val _name : Lens[Person, String] = ???
  val _age : Lens[Person, Int] = ???
  val _contactType : Lens[Person, ContactType] = ???

  val _Business : Prism[ContactType, Unit] = ???
  val _Personal : Prism[ContactType, Unit] = ???

  def encodePerson(v: Person): JSON = ???

  def decodePerson(v: JSON): Option[Person] = ???
}

object exercise6 {
  sealed trait JSON[A]
  // ???

  val TraverseJson: Traverse[JSON] = ???
}

object exercise7 {
  import exercise6._

  type RecursiveJSON = Fix[JSON]

  val ExampleJSON : RecursiveJSON = ???

  def detectStringNumbers(v: RecursiveJSON): RecursiveJSON = {
    val functorT: FunctorT[Fix] = implicitly[FunctorT[Fix]]

    import functorT._


    ???
  }
}
