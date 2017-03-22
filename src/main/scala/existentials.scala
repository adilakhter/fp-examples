object Main01 extends App {

  case class Stack[F[_], A](left: F[A])

  val a = Stack[Either[String, ?], Int](Right(1))
  println(a)


  val b = Stack[({type L[A] = Either[String, A]})#L, Int](Right(1))
  println(b)


  type L[A] = Either[String, A]
  val c = Stack[L, Int](Right(1))
  println(c)
}

object Main02 extends App {

  // In universal type => i choose the type
  // caller -> each of the caller can choose different type of A
  trait List[A]

  def identity[A](v: A) = v

  // in existential type, I dont get to choose what is type . They do.
  trait FS {
    type File

    // -> existential type
    def ls: List[File]
  }

  // val fs: LinuxFS = LinuxFS{...}
  // code is polymorphic by type.
  // fs#File => will be the return type of LS
  // scala compiler will force the client to write polymorphic function.

  // Advice: write as polymorphic code as possible -> it reduces the number of ways to
  // implement a function.

  // there is so many ways to implement the following function :
  // the following could be source of the bug:
  def zip[A](l: List[A])(r: List[A]) : List[A] = ???

  // more polymorphic implementation:
  def zip[A,B, C](l: List[A])(r: List[B])(both: (A , B)=> C, leftEmpty: B => C, rightEmpty: A => C) : List[C] = ???
  // there is only one way to implement it -> in a correct way
  // due to its polymophicness

}