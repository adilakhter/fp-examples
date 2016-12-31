package object xiaon {

  object reflectionUtils {

    import scala.reflect.runtime.{universe => ru}

    def getTypeTag[T: ru.TypeTag](obj: T): ru.TypeTag[T] =
      ru.typeTag[T]

    def transformToEventClassTransformer[A,B](fn: Function1[A,B])
                                             (implicit typeTag: ru.TypeTag[Function1[A,B]]): (Class[_], Class[_]) = {

      val fnTypeTag = getTypeTag(fn)
      val typeArgs = fnTypeTag.tpe.typeArgs
      val runtimeMirror = fnTypeTag.mirror

      val (in, out) = (typeArgs.head, typeArgs.last)

      val inputType = runtimeMirror.runtimeClass(in)
      val outputType = runtimeMirror.runtimeClass(out)

      (inputType, outputType)
    }
  }
}
