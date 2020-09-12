package com.abstractcode.eventrunner

import cats.data.{Ior, Kleisli}
import cats.syntax.all._
import com.abstractcode.eventrunner.MessageProcessor.{MessageContainerWithFinaliser, MessageSource}
import com.abstractcode.eventrunner.TestMessage.TestContainer
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction1
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class MessageProcessorSpec extends Specification with ScalaCheck {
  type Test[A] = Ior[List[String], A]
  type TestKleisli[A] = Kleisli[Test, TestMetadata, A]

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

  val alwaysSuccessHandler: MessageHandler[TestKleisli, TestMessage] = _ => Kleisli(_ => ().rightIor)
  val testMessageContainer: TestContainer = MessageContainer(FirstTestMessage("test"), TestMetadata("transaction", "test"))
  val messageContainerWithFinaliser: MessageContainerWithFinaliser[Test, TestContainer] = MessageContainerWithFinaliser(testMessageContainer, () => ().rightIor)
  val testSource: MessageSource[Test, TestContainer] = () => Some(messageContainerWithFinaliser).rightIor

  def shouldProcessSuccessfully: MatchResult[Boolean] = {
    val processor = new MessageProcessor[Test, TestMessage, TestMetadata](testSource, alwaysSuccessHandler)

    val result = processor.process()

    result.isRight should beTrue
  }

  def shouldReceiveExpectedMessage: ScalaCheckFunction1[String, MatchResult[Option[List[String]]]] =
  prop {
    (message: String) => {
      val handler: MessageHandler[TestKleisli, TestMessage] = {
        case FirstTestMessage(message) => Kleisli(_ => Ior.both(List(message), ()))
        case _ => Kleisli(_ => Ior.left(List("Unknown test message")))
      }

      val messageContainer: TestContainer = MessageContainer(FirstTestMessage(message), TestMetadata("transaction", "test"))
      val withFinaliser: MessageContainerWithFinaliser[Test, TestContainer] = MessageContainerWithFinaliser(messageContainer, () => ().rightIor)
      val testSource: MessageSource[Test, TestContainer] = () => Some(withFinaliser).rightIor

      val processor = new MessageProcessor[Test, TestMessage, TestMetadata](testSource, handler)

      val result = processor.process()

      result.left should beSome(List(message))
    }
  }

  def shouldFailIfMessageSourceFails: MatchResult[Option[List[String]]] = {
    val processor = new MessageProcessor[Test, TestMessage, TestMetadata](() => List("failed").leftIor, alwaysSuccessHandler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldFailIfProcessingFails: MatchResult[Option[List[String]]] = {
    val handler: MessageHandler[TestKleisli, TestMessage] = _ => Kleisli(_ => List("failed").leftIor)

    val processor = new MessageProcessor[Test, TestMessage, TestMetadata](testSource, handler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldFailIfFinalisationFails: MatchResult[Option[List[String]]] = {
    val failingMessageContainerWithFinaliser: MessageContainerWithFinaliser[Test, TestContainer] = MessageContainerWithFinaliser[Test, TestContainer](testMessageContainer, () => List("failed").leftIor)

    val processor = new MessageProcessor[Test, TestMessage, TestMetadata](() => Some(failingMessageContainerWithFinaliser).rightIor, alwaysSuccessHandler)

    val result = processor.process()

    result.left should beSome(List("failed"))
  }

  def shouldCallOperationsInExpectedOrder: MatchResult[Option[List[String]]] = {
    val testLoggingFinaliser: MessageContainerWithFinaliser[Test, TestContainer] = MessageContainerWithFinaliser(testMessageContainer, () => Ior.both(List("finalised"), ()))

    val processor = new MessageProcessor[Test, TestMessage, TestMetadata](() => Some(testLoggingFinaliser).rightIor, _ => Kleisli(_ => Ior.both(List("handled"), ())))

    val result = processor.process()

    result.left should beSome(List("handled", "finalised"))
  }

  def shouldReturnOnlySuccessIfNoMessageIsReceived: MatchResult[Boolean] = {
    val processor = new MessageProcessor[Test, TestMessage, TestMetadata](() => None.rightIor, _ => Kleisli(_ => List("handled").leftIor))

    val result = processor.process()

    result.isRight should beTrue
  }
}
