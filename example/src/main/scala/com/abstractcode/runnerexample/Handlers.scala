package com.abstractcode.runnerexample

import com.abstractcode.eventrunner.logging.Logged
import com.abstractcode.runnerexample.ExampleMessages._

object Handlers {
  def firstMessageHandler[F[_]: Logged]: PartialFunction[ExampleMessage, F[Unit]] = {
    case FirstExampleMessage(count) => Logged[F].log(s"First Count: $count")
  }

  def secondMessageHandler[F[_]: Logged]: PartialFunction[ExampleMessage, F[Unit]] = {
    case SecondExampleMessage(count) => Logged[F].log(s"Second Count: $count")
  }
}
