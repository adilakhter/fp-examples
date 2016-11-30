import collection.mutable
import DynamicExecutionContext._

object Test extends App {



  val x = new DynamicExecutionContext
  x.updateDynamic("lookup1")((x: String, y: Int) => s"A method called with arg1(string)=$x and arg2(int)=$y")
  x.lookup2 = (x: String) => s"42"

  println(x.applyDynamic("lookup1")("hello", 42))

  println(x.lookup2("hello"))




}




class DynamicExecutionContext extends Dynamic {

  private val methods = mutable.Map.empty[String, GenFn] withDefault { key => throw new NoSuchMethodError(key) }

  def selectDynamic(key: String) = methods(key)

  def updateDynamic(key: String)(value: Any) = value match {
    case fn0: Function0[Any] => methods(key) = {case Seq() => fn0()}
    case fn1: Function1[Any, Any] => methods(key) = fn1
    case fn2: Function2[Any, Any, Any] => println(key); methods(key) = fn2
    case _ => throw new UnsupportedOperationException()
  }

  def applyDynamic(key: String)(args: Any*) = methods(key)(args)


  def delete(key: String) = throw new UnsupportedOperationException("Delete not supported")

}

object DynamicExecutionContext {
  import reflect.ClassTag

  type GenFn = PartialFunction[Seq[Any],Any]
  implicit def toGenFn1[A: ClassTag](f: (A) => Any): GenFn = { case Seq(a: A) => f(a) }
  implicit def toGenFn2[A: ClassTag, B: ClassTag](f: (A, B) => Any): GenFn = { case Seq(a: A, b: B) => f(a, b) }


  def compose[A,B,C](f: PartialFunction[A, B], g: PartialFunction[B, C]) : PartialFunction[A, C] =
    Function.unlift(f.andThen(g.lift))
}


