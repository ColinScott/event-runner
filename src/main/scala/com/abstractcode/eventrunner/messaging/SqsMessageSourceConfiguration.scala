package com.abstractcode.eventrunner.messaging

import cats.Show
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.implicits._
import com.abstractcode.eventrunner.messaging.ParseError.{Reason, _}
import com.abstractcode.eventrunner.messaging.SqsMessageSourceConfiguration.{SqsEnvironment, WaitTime}
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import org.http4s.Uri
import software.amazon.awssdk.auth.credentials.{AwsBasicCredentials, AwsCredentials}

case class SqsMessageSourceConfiguration(queueUri: Uri, sqsEnvironment: SqsEnvironment, waitTime: WaitTime)

object SqsMessageSourceConfiguration {

  sealed trait SqsEnvironment

  case object SqsProduction extends SqsEnvironment

  case class SqsLocalstack(baseUri: Uri, credentials: AwsCredentials) extends SqsEnvironment

  type WaitTimeConstraint = Interval.Closed[W.`0`.T, W.`20`.T]
  type WaitTime = Int Refined WaitTimeConstraint

  def getOptionalUri(env: Map[String, String])(key: String): ValidatedNec[ParseError, Option[Uri]] =
    env.get(key)
      .traverse(e => Uri.fromString(e).leftMap(_ => ParseError(key, InvalidFormat)).toValidatedNec)

  def getUri(env: Map[String, String])(key: String): ValidatedNec[ParseError, Uri] = env.get(key)
    .filter(!_.isBlank)
    .map(e => Uri.fromString(e).leftMap(_ => ParseError(key, InvalidFormat)).toValidatedNec)
    .getOrElse(ParseError(key, NotProvidedOrEmpty).invalidNec)


  def apply(env: Map[String, String]): ValidatedNec[ParseError, SqsMessageSourceConfiguration] = {
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

    (getUri(env)("QUEUE_URL"), getSqsEnvironment, getWaitTime).mapN(SqsMessageSourceConfiguration.apply)
  }
}

case class ParseError(environmentVariable: String, reason: Reason)
case class ParseErrors(errors: NonEmptyChain[ParseError]) extends Exception

object ParseError {

  sealed trait Reason
  case object NotProvidedOrEmpty extends Reason
  case object InvalidFormat extends Reason

  implicit val showParseError: Show[ParseError] = Show.show {
    case ParseError(variable, NotProvidedOrEmpty) => s"Environment variable $variable was not provided or is empty"
    case ParseError(variable, InvalidFormat) => s"Format of environment variable $variable is invalid"
  }
}