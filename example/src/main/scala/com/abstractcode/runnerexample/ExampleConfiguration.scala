package com.abstractcode.runnerexample

import cats.data.ValidatedNec
import cats.syntax.all._
import com.abstractcode.eventrunner.configuration.{ParseError, getUriNec}
import com.abstractcode.eventrunner.messaging.SqsMessageSourceConfiguration
import org.http4s.Uri

case class ExampleConfiguration(queueUri: Uri, sqsMessageSourceConfiguration: SqsMessageSourceConfiguration)

object ExampleConfiguration {
  def parse(env: Map[String, String]): ValidatedNec[ParseError, ExampleConfiguration] =
    (getUriNec(env)("QUEUE_URI"), SqsMessageSourceConfiguration.parse(env))
      .mapN(ExampleConfiguration.apply)
}