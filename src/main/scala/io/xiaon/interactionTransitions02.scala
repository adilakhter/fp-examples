package io.xiaon

import java.lang.reflect.Method

object polyfunctionsexamples02 extends App {

  case class InteractionTransition[A](interactionClass: Class[A], interactionProvider: () => A, interactionMethod: String) {

    val method: Method = interactionClass.getDeclaredMethods.find(_.getName == interactionMethod)
      .getOrElse(throw new IllegalStateException(s"No method named '$interactionMethod' defined on '${interactionClass.getName}'"))

    val interactionObject: A = interactionProvider.apply()

    def invokeInteraction(args: Seq[AnyRef]): Object = {
      method.invoke(interactionObject, args: _*)
    }
  }

  object interactionsDef {

    import xiaon.reflectionUtils._

    val interactionTransition = InteractionTransition[SimpleInteractionThatTriggersEvent](
      interactionClass = classOf[SimpleInteractionThatTriggersEvent],
      interactionProvider = () => SimpleInteractionThatTriggersEventImpl,
      interactionMethod = "aInteraction")


    case class EventFromInteraction(msg: String)

    case class TransformedEventFromInteraction(msg: String, s: Int)

    val testEventTransformer: PartialFunction[EventFromInteraction, TransformedEventFromInteraction] = {
      case EventFromInteraction(msg) ⇒ TransformedEventFromInteraction(msg, Integer.MAX_VALUE)
    }

    val testEventClassTransformer: PartialFunction[Class[_], Class[_]] = ???

    trait SimpleInteractionThatTriggersEvent {
      def aInteraction(message: String): EventFromInteraction
    }

    case object SimpleInteractionThatTriggersEventImpl extends SimpleInteractionThatTriggersEvent {
      override def aInteraction(message: String): EventFromInteraction =
        EventFromInteraction(message)
    }

  }


  import interactionsDef._

  val result = interactionTransition.invokeInteraction(Seq("this is hello world"))

  println(result)
}