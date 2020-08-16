package com.abstractcode.eventrunner

import cats.data.Ior
import cats.implicits._
import com.abstractcode.eventrunner.MessageProcessor.MessageHandler
import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

case class TestMessage(message: String)

class MessageProcessorSpec extends Specification {
  type Test[A] = Ior[List[String], A]

  def is: SpecStructure =
    s2"""
         Message Processor should
            not raise error for successful processing $shouldReturnOnlySuccessOnSuccessfulProcessing
            fail if message source fails $shouldFailIfMessageSourceFails
            fail if message processing fails $shouldFailIfProcessingFails
            fail if message finalisation fails $shouldFailIfFinalisationFails
            order operations correctly $shouldCallOperationsInExpectedOrder
            return only success if no message is received $shouldReturnOnlySuccessIfNoMessageIsReceived
      """

  val alwaysSuccessHandler: MessageHandler[Test, TestMessage] = { case _: TestMessage => ().rightIor }
  val testMessageContainer: MessageContainer[Test, TestMessage] = MessageContainer[Test, TestMessage](TestMessage("test"), () =>().rightIor)
  val testSource: () => Test[Option[MessageContainer[Test, TestMessage]]] = () => Some(testMessageContainer).rightIor

  def shouldReturnOnlySuccessOnSuccessfulProcessing: MatchResult[Boolean] = {
    val processor = new MessageProcessor[Test, TestMessage](testSource, alwaysSuccessHandler)

    val result = processor.process()

    result.isRight should beTrue
  }

  def shouldFailIfMessageSourceFails: MatchResult[Option[List[String]]] = {
    val processor = new MessageProcessor[Test, TestMessage](() => List("failed").leftIor, alwaysSuccessHandler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldFailIfProcessingFails: MatchResult[Option[List[String]]] = {
    val handler: MessageHandler[Test, TestMessage] = { case _ => List("failed").leftIor }

    val processor = new MessageProcessor[Test, TestMessage](testSource, handler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldFailIfFinalisationFails: MatchResult[Option[List[String]]] = {
    val testMessageContainer: MessageContainer[Test, TestMessage] = MessageContainer[Test, TestMessage](TestMessage("test"), () => List("failed").leftIor)

    val processor = new MessageProcessor[Test, TestMessage](() => Some(testMessageContainer).rightIor, alwaysSuccessHandler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldCallOperationsInExpectedOrder: MatchResult[Option[List[String]]] = {
    val testMessageContainer: MessageContainer[Test, TestMessage] = MessageContainer[Test, TestMessage](TestMessage("test"), () => Ior.both(List("finalised"), ()))

    val processor = new MessageProcessor[Test, TestMessage](() => Some(testMessageContainer).rightIor, {
      case _ => Ior.both(List("handled"), ())
    })

    val result = processor.process()

    result.left should beSome(List("handled", "finalised"))
  }

  def shouldReturnOnlySuccessIfNoMessageIsReceived: MatchResult[Boolean] = {
    val processor = new MessageProcessor[Test, TestMessage](() => None.rightIor, {
      case _ => List("handled").leftIor
    })

    val result = processor.process()

    result.isRight should beTrue
  }
}
