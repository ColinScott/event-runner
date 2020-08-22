package com.abstractcode.runnerexample

import cats.data.ValidatedNec
import cats.implicits._
import com.abstractcode.eventrunner.messaging.SqsMessageSourceConfiguration
import com.abstractcode.eventrunner.configuration.{ParseError, getUri}
import org.http4s.Uri

case class ExampleConfiguration(queueUri: Uri, sqsMessageSourceConfiguration: SqsMessageSourceConfiguration)

object ExampleConfiguration {
  def parse(env: Map[String, String]): ValidatedNec[ParseError, ExampleConfiguration] = {
    (getUri(env)("QUEUE_URI"), SqsMessageSourceConfiguration.parse(env)).mapN(ExampleConfiguration.apply)
  }
}