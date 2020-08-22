package com.abstractcode.eventrunner.messaging

import cats.data.ValidatedNec
import cats.implicits._
import com.abstractcode.eventrunner.configuration.getOptionalUri
import com.abstractcode.eventrunner.configuration.ParseError
import com.abstractcode.eventrunner.configuration.ParseError.{InvalidFormat, NotProvidedOrEmpty}
import com.abstractcode.eventrunner.messaging.SqsMessageSourceConfiguration.{SqsEnvironment, WaitTime}
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import org.http4s.Uri
import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, AwsCredentials}

case class SqsMessageSourceConfiguration(sqsEnvironment: SqsEnvironment, waitTime: WaitTime)

object SqsMessageSourceConfiguration {

  sealed trait SqsEnvironment

  case object SqsProduction extends SqsEnvironment

  case class SqsLocalstack(baseUri: Uri, credentials: AwsCredentials) extends SqsEnvironment

  type WaitTimeConstraint = Interval.Closed[W.`0`.T, W.`20`.T]
  type WaitTime = Int Refined WaitTimeConstraint

  def parse(env: Map[String, String]): ValidatedNec[ParseError, SqsMessageSourceConfiguration] = {
    def getSqsEnvironment: ValidatedNec[ParseError, SqsEnvironment] = {
      val sqsUri = getOptionalUri(env)("LOCALSTACK_SQS_URI")
      val accessKeyId = env.get("LOCALSTACK_ACCESS_KEY_ID").validNec
      val secretAccessKey = env.get("LOCALSTACK_SECRET_ACCESS_KEY").validNec

      (sqsUri, accessKeyId, secretAccessKey).mapN {
        case (None, None, None) => SqsProduction.validNec
        case (Some(uri), Some(keyId), Some(secret)) => SqsLocalstack(uri, AwsBasicCredentials.create(keyId, secret)).validNec
        case _ => ParseError("LOCALSTACK_SQS_URI", InvalidFormat).invalidNec
      }.fold(e => e.invalid, a => a)
    }

    def getWaitTime: ValidatedNec[ParseError, WaitTime] = (for {
      waitEnvironment <- env.get("QUEUE_WAIT_TIME").toRight(ParseError("QUEUE_WAIT_TIME", NotProvidedOrEmpty))
      value <- waitEnvironment.toIntOption.toRight(ParseError("QUEUE_WAIT_TIME", InvalidFormat))
      waitTime <- refineV[WaitTimeConstraint](value)
        .leftMap(_ => ParseError("QUEUE_WAIT_TIME", InvalidFormat))
    } yield waitTime).toValidatedNec

    (getSqsEnvironment, getWaitTime).mapN(SqsMessageSourceConfiguration.apply)
  }
}