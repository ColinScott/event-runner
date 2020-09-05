package com.abstractcode.eventrunner.messaging

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Chain, Validated, ValidatedNec}
import cats.syntax.all._
import com.abstractcode.eventrunner.configuration.ParseError.{InvalidFormat, InvalidSection, NotProvidedOrEmpty}
import com.abstractcode.eventrunner.configuration.{ParseError, getString, getUri}
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
      val getEnvString: String => Validated[ParseError, String] = getString(env)

      val sqsUri = getUri(env)("LOCALSTACK_SQS_URI")
      val accessKeyId = getEnvString("LOCALSTACK_ACCESS_KEY_ID")
      val secretAccessKey = getEnvString("LOCALSTACK_SECRET_ACCESS_KEY")

      (sqsUri, accessKeyId, secretAccessKey) match {
        case (Invalid(NotProvidedOrEmpty(_)), Invalid(NotProvidedOrEmpty(_)), Invalid(NotProvidedOrEmpty(_))) => SqsProduction.validNec
        case (Valid(uri), Valid(keyId), Valid(secret)) => SqsLocalstack(uri, AwsBasicCredentials.create(keyId, secret)).validNec
        case (e1, e2, e3) => InvalidSection(
          "LocalStack configuration invalid",
          Chain(e1, e2, e3).flatMap {
            case Invalid(e) => Chain(e)
            case _ => Chain.empty
          }
        ).invalidNec
      }
    }

    def getWaitTime: ValidatedNec[ParseError, WaitTime] = (for {
      waitEnvironment <- env.get("QUEUE_WAIT_TIME").toRight(NotProvidedOrEmpty("QUEUE_WAIT_TIME"))
      value <- waitEnvironment.toIntOption.toRight(InvalidFormat("QUEUE_WAIT_TIME"))
      waitTime <- refineV[WaitTimeConstraint](value)
        .leftMap(_ => InvalidFormat("QUEUE_WAIT_TIME"))
    } yield waitTime).toValidatedNec

    (getSqsEnvironment, getWaitTime).mapN(SqsMessageSourceConfiguration.apply)
  }
}