package com.abstractcode.eventrunner.messaging

import java.net.URI

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import com.abstractcode.eventrunner.messaging.SqsMessageSourceConfiguration.{SqsLocalstack, SqsProduction}
import com.abstractcode.eventrunner.{Message, MessageContainer}
import org.http4s.Uri
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider
import software.amazon.awssdk.services._
import software.amazon.awssdk.services.sqs.SqsClient
import software.amazon.awssdk.services.sqs.model.{DeleteMessageRequest, ReceiveMessageRequest}

import scala.jdk.CollectionConverters._

object SqsMessageSource {
  def retrieveMessage[F[_] : Sync](configuration: SqsMessageSourceConfiguration)(queueUri: Uri)(messageParser: sqs.model.Message => F[Message]): F[() => F[Option[MessageContainer[F]]]] = {
    def buildSqsClient: F[SqsClient] = Sync[F].delay {
      val builder = SqsClient.builder()
      configuration.sqsEnvironment match {
        case SqsProduction => builder.build()
        case SqsLocalstack(baseUri, credentials) =>
          builder.endpointOverride(new URI(baseUri.renderString))
            .credentialsProvider(StaticCredentialsProvider.create(credentials))
            .build()
      }
    }

    buildSqsClient
      .map(
        client => {
          val queueUriString = queueUri.renderString

          val receiveRequest = ReceiveMessageRequest.builder()
            .queueUrl(queueUriString)
            .maxNumberOfMessages(1)
            .waitTimeSeconds(configuration.waitTime.value)
            .build()

          val receiveMessage: F[Option[sqs.model.Message]] = Sync[F].delay {
            client.receiveMessage(receiveRequest).messages().asScala.headOption
          }

          def deleteMessage(message: sqs.model.Message): F[Unit] = Sync[F].delay(
            client.deleteMessage(
              DeleteMessageRequest.builder()
                .queueUrl(queueUriString)
                .receiptHandle(message.receiptHandle())
                .build()
            )).map(_ => ())

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
