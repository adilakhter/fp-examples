package io.xiaon

import java.lang.reflect.Method
import scala.reflect.ClassTag

case class InteractionTransition4[A](interactionClass: Class[A],
                                     interactionProvider: () => A,
                                     interactionMethod: String,
                                     eventResultTransformer: Map[Class[_], EventOutputTransformer[_, _]]) {
  val method: Method =
    interactionClass
      .getDeclaredMethods
      .find(_.getName == interactionMethod)
      .getOrElse(throw new IllegalStateException(s"No method named '$interactionMethod' defined on '${interactionClass.getName}'"))

  val interactionObject: A = interactionProvider.apply()

  def invokeInteraction(args: Seq[AnyRef]) = {
    val result = method.invoke(interactionObject, args: _*)

    eventResultTransformer
      .get(result.getClass)
      .fold(result)(_.transform(result).asInstanceOf[AnyRef])
  }
}


case class EventFromInteraction(msg: String)

case class TransformedEventFromInteraction(msg: String, s: Int)

trait SimpleInteractionThatTriggersEvent {
  def aInteraction(message: String): EventFromInteraction
}

case object SimpleInteractionThatTriggersEventImpl extends SimpleInteractionThatTriggersEvent {
  override def aInteraction(message: String): EventFromInteraction =
    EventFromInteraction(message)
}

abstract class EventOutputTransformer[A: ClassTag, B: ClassTag] {
  def sourceType: Class[_] = implicitly[ClassTag[A]].runtimeClass

  def targetType: Class[_] = implicitly[ClassTag[B]].runtimeClass

  def fn: A ⇒ B

  def transform(a: AnyRef): B = fn(a.asInstanceOf[A])

  override def toString: String = s"${sourceType.getSimpleName} ⇒ ${targetType.getSimpleName}"
}

object EventResultTransformerOps {

  implicit def fnToEventTransformer[A: ClassTag, B: ClassTag](aFunction: A ⇒ B): EventOutputTransformer[A, B] = {
    new EventOutputTransformer[A, B] {
      def fn: (A) => B = aFunction
    }
  }
}

object interactionTransitionExampleApp extends App {
  import EventResultTransformerOps._

  // Transformation Functions
  val fn1: EventFromInteraction ⇒ String = _.msg
  val fn2: TransformedEventFromInteraction ⇒ EventFromInteraction = t ⇒ EventFromInteraction(t.msg)

  // implicitly converting to eventOutputTranformers
  val ert3: EventOutputTransformer[_, _] = (a: EventFromInteraction) ⇒ TransformedEventFromInteraction(a.msg, 10)
  val mapper: Map[Class[_], EventOutputTransformer[_, _]] = Map(fn1.sourceType -> fn1, fn2.sourceType -> fn2)
  // updating a transformation function
  val mapper2: Map[Class[_], EventOutputTransformer[_, _]] = mapper + (ert3.sourceType -> ert3)


  // creating an interaction
  val aInteraction =
    InteractionTransition4[SimpleInteractionThatTriggersEvent](
      interactionClass = classOf[SimpleInteractionThatTriggersEvent],
      interactionProvider = () => SimpleInteractionThatTriggersEventImpl,
      interactionMethod = "aInteraction",
      eventResultTransformer = mapper2)

  // invoking the interaction
  val result = aInteraction.invokeInteraction(args = Seq("this is hello world"))
  println(result)
}