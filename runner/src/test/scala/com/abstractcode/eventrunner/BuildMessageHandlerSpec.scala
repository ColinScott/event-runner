package com.abstractcode.eventrunner

import cats.data.{Ior, Kleisli}
import cats.syntax.all._
import com.abstractcode.eventrunner.TestMessage._
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.{ScalaCheckFunction1, ScalaCheckFunction2}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class BuildMessageHandlerSpec extends Specification with ScalaCheck {
  type Test[A] = Ior[List[String], A]

  def is: SpecStructure =
    s2"""
         buildMessageHandler should
            succeed with handling message PartialFunction supports $succeedWhenMessageTypeIsSupported
            handle message via handler if PartialFunction supports it $useHandlerWhenMessageTypeIsSupported
            pass expected metadata $passExpectedMetadata
            handle message by fallback handler if PartialFunction does not support it $handleWithFallbackWhenPartialFunctionDoesNotSupportMessage
      """

  val fallbackFail: TestMessage => MessageContext[Test, TestMetadata] = _ => Kleisli(_ => List("fallback fail").leftIor)

  def succeedWhenMessageTypeIsSupported: ScalaCheckFunction1[TestContainer, MatchResult[Boolean]] =
    prop {
      (testContainer: TestContainer) => {

        val handler: PartialFunction[TestMessage, MessageContext[Test, TestMetadata]] = {
          case _ => Kleisli(_ => ().rightIor)
        }

        val messageHandler: ContainerHandler[Test, MessageContainer[TestMessage, TestMetadata]] =
          buildMessageHandler[Test, TestMessage, TestMetadata](handler, fallbackFail)

        val result = messageHandler(testContainer)

        result.isRight should beTrue
      }
    }

  def useHandlerWhenMessageTypeIsSupported: ScalaCheckFunction2[FirstTestMessage, TestMetadata, MatchResult[Any]] =
    prop {
      (testMessage: FirstTestMessage, testMetadata: TestMetadata) => {

        val handler: PartialFunction[TestMessage, MessageContext[Test, TestMetadata]] = {
          case FirstTestMessage(message) => Kleisli(_ => Ior.both(List(message), ()))
        }

        val messageHandler: ContainerHandler[Test, MessageContainer[TestMessage, TestMetadata]] =
          buildMessageHandler[Test, TestMessage, TestMetadata](handler, fallbackFail)

        val messageContainer = MessageContainer(testMessage, testMetadata)
        val result = messageHandler(messageContainer)

        result.left shouldEqual Some(List(testMessage.message))
      }
    }

  def passExpectedMetadata: ScalaCheckFunction2[FirstTestMessage, TestMetadata, MatchResult[Any]] =
    prop {
      (testMessage: FirstTestMessage, testMetadata: TestMetadata) => {

        val handler: PartialFunction[TestMessage, MessageContext[Test, TestMetadata]] = {
          case _ => Kleisli {
            case `testMetadata` => ().rightIor
            case _ => List("metadata doesn't match").leftIor
          }
        }

        val messageHandler: ContainerHandler[Test, MessageContainer[TestMessage, TestMetadata]] =
          buildMessageHandler[Test, TestMessage, TestMetadata](handler, fallbackFail)

        val messageContainer = MessageContainer(testMessage, testMetadata)
        val result = messageHandler(messageContainer)

        result.isRight should beTrue
      }
    }

  def handleWithFallbackWhenPartialFunctionDoesNotSupportMessage: ScalaCheckFunction2[FirstTestMessage, TestMetadata, MatchResult[Any]] =
    prop {
      (testMessage: FirstTestMessage, testMetadata: TestMetadata) => {
        val handler: PartialFunction[TestMessage, MessageContext[Test, TestMetadata]] = PartialFunction.empty

        val fallback: TestMessage => MessageContext[Test, TestMetadata] = {
          case `testMessage` => Kleisli {
            case `testMetadata` => Ior.both(List("fallback"), ())
            case _ => List("metadata doesn't match").leftIor
          }
          case _ => Kleisli.liftF(List("message doesn't match").leftIor)
        }

        val messageHandler: ContainerHandler[Test, MessageContainer[TestMessage, TestMetadata]] =
          buildMessageHandler[Test, TestMessage, TestMetadata](handler, fallback)

        val messageContainer = MessageContainer(testMessage, testMetadata)
        val result = messageHandler(messageContainer)

        result.left shouldEqual Some(List("fallback"))
      }
    }
}
