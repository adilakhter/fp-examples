package essenceoffp


object totalfunctions01 extends App{

  def maxValue(xs: List[Int]): String =
    s"Maximum value is ${xs.max}"

  val ints = List.empty[Int]

  maxValue(ints)
}


object totalfunction02 extends App {

  def divideOneBy(y: Int): Double = 1/y

  divideOneBy (0)
}

object totalfunctions03 extends App {

  sealed trait Option[A]
  case class Some[A](value: A) extends Option[A]
  case class None[A]()         extends Option[A]

  sealed trait \/[A, B]
  case class -\/ [A, B](value: A) extends \/[A, B]
  case class  \/-[A, B](value: B) extends \/[A, B]

  type Either[A, B] = A \/ B
}