package com.abstractcode.eventrunner

import cats.data.Ior
import cats.implicits._
import com.abstractcode.eventrunner.MessageProcessor.{MCF, MessageSource}
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction1
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

case class TestMessage(message: String)
case class TestMetadata(transactionId: String, messageType: String) extends Metadata[String, String]

class MessageProcessorSpec extends Specification with ScalaCheck {
  type Test[A] = Ior[List[String], A]

  def is: SpecStructure =
    s2"""
         Message Processor should
            process successfully $shouldProcessSuccessfully
            receive process expected message $shouldReceiveExpectedMessage
            fail if message source fails $shouldFailIfMessageSourceFails
            fail if message processing fails $shouldFailIfProcessingFails
            fail if message finalisation fails $shouldFailIfFinalisationFails
            order operations correctly $shouldCallOperationsInExpectedOrder
            return only success if no message is received $shouldReturnOnlySuccessIfNoMessageIsReceived
      """

  type TestContainer = MessageContainer[TestMessage, TestMetadata]
  val alwaysSuccessHandler: MessageHandler[Test, TestContainer] = _ => ().rightIor
  val testMessageContainer: TestContainer = MessageContainer(TestMessage("test"), TestMetadata("transaction", "test"))
  val messageContainerWithFinaliser: MCF[Test, TestContainer] = MCF(testMessageContainer, () => ().rightIor)
  val testSource: MessageSource[Test, TestContainer] = () => Some(messageContainerWithFinaliser).rightIor

  def shouldProcessSuccessfully: MatchResult[Boolean] = {
    val processor = new MessageProcessor[Test, TestContainer](testSource, alwaysSuccessHandler)

    val result = processor.process()

    result.isRight should beTrue
  }

  def shouldReceiveExpectedMessage: ScalaCheckFunction1[String, MatchResult[Option[List[String]]]] =
  prop {
    (message: String) => {
      val handler: MessageHandler[Test, TestContainer] = received => Ior.both(List(received.message.message), ())

      val messageContainer: TestContainer = MessageContainer(TestMessage(message), TestMetadata("transaction", "test"))
      val withFinaliser: MCF[Test, TestContainer] = MCF(messageContainer, () => ().rightIor)
      val testSource: MessageSource[Test, TestContainer] = () => Some(withFinaliser).rightIor

      val processor = new MessageProcessor[Test, TestContainer](testSource, handler)

      val result = processor.process()

      result.left should beSome(List(message))
    }
  }

  def shouldFailIfMessageSourceFails: MatchResult[Option[List[String]]] = {
    val processor = new MessageProcessor[Test, TestContainer](() => List("failed").leftIor, alwaysSuccessHandler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldFailIfProcessingFails: MatchResult[Option[List[String]]] = {
    val handler: MessageHandler[Test, TestContainer] = _ => List("failed").leftIor

    val processor = new MessageProcessor[Test, TestContainer](testSource, handler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldFailIfFinalisationFails: MatchResult[Option[List[String]]] = {
    val failingMessageContainerWithFinaliser: MCF[Test, TestContainer] = MCF[Test, TestContainer](testMessageContainer, () => List("failed").leftIor)

    val processor = new MessageProcessor[Test, TestContainer](() => Some(failingMessageContainerWithFinaliser).rightIor, alwaysSuccessHandler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldCallOperationsInExpectedOrder: MatchResult[Option[List[String]]] = {
    val testLoggingFinaliser: MCF[Test, TestContainer] = MCF(testMessageContainer, () => Ior.both(List("finalised"), ()))

    val processor = new MessageProcessor[Test, TestContainer](() => Some(testLoggingFinaliser).rightIor, _ => Ior.both(List("handled"), ()))

    val result = processor.process()

    result.left should beSome(List("handled", "finalised"))
  }

  def shouldReturnOnlySuccessIfNoMessageIsReceived: MatchResult[Boolean] = {
    val processor = new MessageProcessor[Test, TestContainer](() => None.rightIor, _ => List("handled").leftIor)

    val result = processor.process()

    result.isRight should beTrue
  }
}
