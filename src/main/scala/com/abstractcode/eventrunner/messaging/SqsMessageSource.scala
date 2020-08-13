package com.abstractcode.eventrunner.messaging

import java.net.URI

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import com.abstractcode.eventrunner.{Message, MessageContainer}
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import org.http4s.Uri
import software.amazon.awssdk.auth.credentials.{AwsCredentials, StaticCredentialsProvider}
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services._
import software.amazon.awssdk.services.sqs.SqsClient
import software.amazon.awssdk.services.sqs.model.{DeleteMessageRequest, ReceiveMessageRequest}

import scala.jdk.CollectionConverters._

object SqsMessageSource {

  sealed trait SqsEnvironment
  case object SqsProduction extends SqsEnvironment
  case class SqsLocalstack(baseUri: Uri, credentials: AwsCredentials) extends SqsEnvironment

  type WaitTimeConstraint = Interval.Closed[W.`0`.T, W.`20`.T]
  type WaitTime = Int Refined WaitTimeConstraint

  case class SqsMessageSourceConfiguration(queueUri: Uri, region: Region, sqsEnvironment: SqsEnvironment, waitTime: WaitTime)

  def retrieveMessage[F[_] : Sync](configuration: SqsMessageSourceConfiguration)(messageParser: sqs.model.Message => F[Message]): F[() => F[Option[MessageContainer[F]]]] = {
    def buildSqsClient: F[SqsClient] = Sync[F].delay {
      val builder = SqsClient.builder().region(configuration.region)
      configuration.sqsEnvironment match {
        case SqsProduction => builder.build()
        case SqsLocalstack(baseUri, credentials) =>
          val credsV2 = StaticCredentialsProvider.create(credentials)
          builder.endpointOverride(new URI(baseUri.renderString)).credentialsProvider(credsV2).build()
      }
    }

    buildSqsClient
      .map(
        client => {
          val queueUri = configuration.queueUri.renderString

          val receiveRequest = ReceiveMessageRequest.builder()
            .queueUrl(queueUri)
            .maxNumberOfMessages(1)
            .waitTimeSeconds(configuration.waitTime.value)
            .build()

          val receiveMessage: F[Option[sqs.model.Message]] = Sync[F].delay {
            client.receiveMessage(receiveRequest).messages().asScala.headOption
          }

          def deleteMessage(message: sqs.model.Message): F[Unit] = Sync[F].delay {
            client.deleteMessage(
              DeleteMessageRequest.builder()
                .queueUrl(queueUri)
                .receiptHandle(message.receiptHandle())
                .build()
            )

            ()
          }

          () => {
            (for {
              sqsMessage <- OptionT(receiveMessage)
              parsedMessage <- OptionT.liftF(messageParser(sqsMessage))
            } yield MessageContainer[F](parsedMessage, () => deleteMessage(sqsMessage))).value
          }
        }
      )
  }

}
