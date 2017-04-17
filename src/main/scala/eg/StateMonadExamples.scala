package eg

import cats.Eval


object MonadicLoopWithStateMonad extends App {
  //https://gist.github.com/noelwelsh/2b27272573c0e5294bb4
  // Monadic Loop with State Monad
}


object ComposingAndTransformingStates extends App {
  import cats.data.State

  val step1 = State[Int, String] { num ⇒
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num ⇒
    val ans = num * 2
    (ans, s"Result of step1: $ans")
  }


  val both =
    for {
      a ← step1
      b ← step2
    } yield (a,b)



  private def printState(evalM: Eval[_]) = {
    val (state, result) = evalM.value

    println("state : " + state)
    println("result: " + result)
  }

  printState(both.run(100))

  import State._

  val program: State[Int, (Int,Int,Int)] = for {
    a ← get[Int]
    _ ← set[Int] (a + 1)
    b ← get[Int]
    _ ← modify[Int](_ + 1)
    c ← inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  printState(program.run(100))
}


object PostOrderCalculator extends App {

  private def printState(evalM: Eval[_]) = {
    val (state, result) = evalM.value

    println("state : " + state)
    println("result: " + result)
  }

  import cats.data.State
  import cats.syntax.applicative._

  type CalcState[A] = State[List[Int], A]


  def op(f: (Int, Int) => Int): State[List[Int], Int] = State[List[Int], Int] {
    case x :: y :: tail ⇒
      val r = f(x, y)
      (r :: tail, r)
    case _ ⇒ sys.error("Error!")
  }

  def evalOne(s: String): CalcState[Int] =
    s match {
      case "+" ⇒ op((a: Int, b: Int) ⇒ a + b)
      case "*" ⇒ op((a: Int, b: Int) ⇒ a * b)
      case "/" ⇒ op((a: Int, b: Int) ⇒ a / b)
      case "-" ⇒ op((a: Int, b: Int) ⇒ a + b)
      case _ ⇒ State[List[Int], Int] { stack ⇒ (s.toInt :: stack, s.toInt) }
    }

  def evalAll(input: List[String]): CalcState[Int] = input.foldLeft(0.pure[CalcState]) { (x, y) ⇒
    x.flatMap(_ ⇒ evalOne(y))
  }

  val program = for {
    _ ← evalAll(List("1", "2",  "+" ))
    _ ← evalAll(List("4", "3",  "+" ))
    r ← evalOne("*")
  } yield r


  printState(program.run(Nil))
}