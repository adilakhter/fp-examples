package object xiaon {

  object reflectionUtils {

    import scala.reflect.runtime.{universe => ru}

    def getTypeTag[T: ru.TypeTag](obj: T): ru.TypeTag[T] =
      ru.typeTag[T]


    /**
      *  Converts a PartialFunction of A ⇒ B to a Partial Function of Class[A] ⇒ Class[B]
      */
    def transformToEventClassTransformer[A,B](eventTransformer: PartialFunction[A,B])
                                             (implicit typeTag: ru.TypeTag[PartialFunction[A,B]]): PartialFunction[Class[_], Class[_]] = {

      //TODO: see if we can somehow avoid using reflection.
      //TODO: used ClassTag of A and B as a context bound

      val fnTypeTag = getTypeTag(eventTransformer)
      val typeArgs = fnTypeTag.tpe.typeArgs
      val runtimeMirror = fnTypeTag.mirror

      val (in, out) = (typeArgs.head, typeArgs.last)

      val inputType = runtimeMirror.runtimeClass(in)
      val outputType = runtimeMirror.runtimeClass(out)

      PartialFunction(inputType ⇒ outputType)
    }

  }
}
