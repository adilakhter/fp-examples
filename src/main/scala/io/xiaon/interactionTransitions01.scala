package io.xiaon

import java.lang.reflect.Method

object reflectionUtils {

  import scala.reflect.runtime.{universe => ru}

  def getTypeTag[T: ru.TypeTag](obj: T): ru.TypeTag[T] =
    ru.typeTag[T]


  /**
    * Transforms eventTransformer, which is a PartialFunction from A ⇒ B
    * to a PartialFunction from classOf[A] ⇒ classOf[B]
    */
  def transformToEventClassTransformer[A,B](eventTransformer: PartialFunction[A,B])
                                           (implicit typeTag: ru.TypeTag[PartialFunction[A,B]]): PartialFunction[Class[_], Class[_]] = {

    val fnTypeTag = getTypeTag(eventTransformer)
    val typeArgs = fnTypeTag.tpe.typeArgs
    val runtimeMirror = fnTypeTag.mirror

    val (in, out) = (typeArgs.head, typeArgs.last)

    val inputType = runtimeMirror.runtimeClass(in)
    val outputType = runtimeMirror.runtimeClass(out)

    PartialFunction(inputType ⇒ outputType)
  }

}

case class InteractionTransition[A](interactionClass: Class[A], interactionProvider: () => A, interactionMethod: String) {

  val method: Method = interactionClass.getDeclaredMethods.find(_.getName == interactionMethod)
    .getOrElse(throw new IllegalStateException(s"No method named '$interactionMethod' defined on '${interactionClass.getName}'"))

  val interactionObject: A = interactionProvider.apply()

  def invokeInteraction(args: Seq[AnyRef]): Object = {
    method.invoke(interactionObject, args: _*)
  }
}

object interactionsDef {

  import reflectionUtils._

  val interactionTransition = InteractionTransition[SimpleInteractionThatTriggersEvent](
    interactionClass = classOf[SimpleInteractionThatTriggersEvent],
    interactionProvider = () => SimpleInteractionThatTriggersEventImpl,
    interactionMethod = "aInteraction")


  case class EventFromInteraction(msg: String)
  case class TransformedEventFromInteraction(msg: String, s: Int)

  val testEventTransformer: PartialFunction[EventFromInteraction, TransformedEventFromInteraction] = {
    case EventFromInteraction(msg) ⇒ TransformedEventFromInteraction(msg, Integer.MAX_VALUE)
  }

  val testEventClassTransformer: PartialFunction[Class[_], Class[_]] =
    transformToEventClassTransformer(testEventTransformer)

  trait SimpleInteractionThatTriggersEvent {
    def aInteraction(message: String): EventFromInteraction
  }

  case object SimpleInteractionThatTriggersEventImpl extends SimpleInteractionThatTriggersEvent {
    override def aInteraction(message: String): EventFromInteraction =
      EventFromInteraction(message)
  }
}


object polyfunctionsexamples01 extends App {

  import interactionsDef._

  val result = interactionTransition.invokeInteraction(Seq("this is hello world"))

  println(result)
}