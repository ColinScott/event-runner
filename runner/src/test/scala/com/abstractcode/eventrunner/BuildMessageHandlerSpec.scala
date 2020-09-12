package com.abstractcode.eventrunner

import cats.data.Ior
import cats.syntax.all._
import com.abstractcode.eventrunner.TestMessage._
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction1
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class BuildMessageHandlerSpec extends Specification with ScalaCheck {
  type Test[A] = Ior[List[String], A]

  def is: SpecStructure =
    s2"""
         buildMessageHandler should
            succeed with handling message PartialFunction supports $succeedWhenMessageTypeIsSupported
            handle message via handler if PartialFunction supports it $useHandlerWhenMessageTypeIsSupported
            handle message by fallback handler if PartialFunction does not support it $handleWithFallbackWhenPartialFunctionDoesNotSupportMessage
      """

  val fallbackFail: TestMessage => Test[Unit] = _ => List("fallback fail").leftIor

  def succeedWhenMessageTypeIsSupported: ScalaCheckFunction1[TestMessage, MatchResult[Boolean]] =
    prop {
      (testMessage: TestMessage) => {

        val handler: PartialFunction[TestMessage, Test[Unit]] = {
          case _ => ().rightIor
        }

        val messageHandler: MessageHandler[Test, TestMessage] =
          buildMessageHandler[Test, TestMessage](handler, fallbackFail)

        val result = messageHandler(testMessage)

        result.isRight should beTrue
      }
    }

  def useHandlerWhenMessageTypeIsSupported: ScalaCheckFunction1[FirstTestMessage, MatchResult[Any]] =
    prop {
      (testMessage: FirstTestMessage) => {

        val handler: PartialFunction[TestMessage, Test[Unit]] = {
          case FirstTestMessage(message) => Ior.both(List(message), ())
        }

        val messageHandler: MessageHandler[Test, TestMessage] =
          buildMessageHandler[Test, TestMessage](handler, fallbackFail)

        val result = messageHandler(testMessage)

        result.left shouldEqual Some(List(testMessage.message))
      }
    }

  def handleWithFallbackWhenPartialFunctionDoesNotSupportMessage: ScalaCheckFunction1[FirstTestMessage, MatchResult[Any]] =
    prop {
      (testMessage: FirstTestMessage) => {
        val handler: PartialFunction[TestMessage, Test[Unit]] = PartialFunction.empty

        val fallback: TestMessage => Test[Unit] = {
          case `testMessage` => Ior.both(List("fallback"), ())
          case _ => List("message doesn't match").leftIor
        }

        val messageHandler: MessageHandler[Test, TestMessage] =
          buildMessageHandler[Test, TestMessage](handler, fallback)

        val result = messageHandler(testMessage)

        result.left shouldEqual Some(List("fallback"))
      }
    }
}
