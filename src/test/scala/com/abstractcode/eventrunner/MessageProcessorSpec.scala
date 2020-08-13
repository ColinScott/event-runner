package com.abstractcode.eventrunner

import cats.data.Ior
import cats.implicits._
import com.abstractcode.eventrunner.MessageProcessor.MessageHandler
import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

case class TestMessage(message: String) extends Message

class MessageProcessorSpec extends Specification {
  type Test[A] = Ior[List[String], A]

  def is: SpecStructure =
    s2"""
         Message Processor should
            not raise error for successful processing $shouldNotRaiseErrorOnSuccess
            fail if message source fails $shouldFailIfMessageSourceFails
            fail if message processing fails $shouldFailIfProcessingFails
            fail if message finalisation fails $shouldFailIfFinalisationFails
            order operations correctly $shouldCallOperationsInExpectedOrder
      """

  val alwaysSuccessHandler: MessageHandler[Test] = { case _: Message => ().rightIor }
  val testMessageContainer: MessageContainer[Test] = MessageContainer[Test](TestMessage("test"), () =>().rightIor)
  val testSource: () => Test[Option[MessageContainer[Test]]] = () => Some(testMessageContainer).rightIor

  def shouldNotRaiseErrorOnSuccess: MatchResult[Option[Unit]] = {
    val processor = new MessageProcessor[Test](testSource, alwaysSuccessHandler)

    val result = processor.process()

    result.right should beSome
  }

  def shouldFailIfMessageSourceFails: MatchResult[Option[List[String]]] = {
    val processor = new MessageProcessor[Test](() => List("failed").leftIor, alwaysSuccessHandler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldFailIfProcessingFails: MatchResult[Option[List[String]]] = {
    val handler: MessageHandler[Test] = { case _ => List("failed").leftIor }

    val processor = new MessageProcessor[Test](testSource, handler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldFailIfFinalisationFails: MatchResult[Option[List[String]]] = {
    val testMessageContainer: MessageContainer[Test] = MessageContainer[Test](TestMessage("test"), () => List("failed").leftIor)

    val processor = new MessageProcessor[Test](() => Some(testMessageContainer).rightIor, alwaysSuccessHandler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldCallOperationsInExpectedOrder: MatchResult[Option[List[String]]] = {
    val testMessageContainer: MessageContainer[Test] = MessageContainer[Test](TestMessage("test"), () => Ior.both(List("finalised"), ()))

    val processor = new MessageProcessor[Test](() => Some(testMessageContainer).rightIor, {
      case _ => Ior.both(List("handled"), ())
    })

    val result = processor.process()

    result.left should beSome(List("handled", "finalised"))
  }
}
