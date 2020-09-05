package com.abstractcode.runnerexample

import com.abstractcode.eventrunner.MessageContext
import com.abstractcode.runnerexample.ExampleMessages._

object Handlers {
  def firstMessageHandler[F[_]: ExampleLogged]: PartialFunction[ExampleMessage, MessageContext[F, ExampleMetadata]] = {
    case FirstExampleMessage(count) => ExampleLogged[F].log(s"First Count: $count")
  }

  def secondMessageHandler[F[_]: ExampleLogged]: PartialFunction[ExampleMessage, MessageContext[F, ExampleMetadata]] = {
    case SecondExampleMessage(count) => ExampleLogged[F].log(s"Second Count: $count")
  }
}
