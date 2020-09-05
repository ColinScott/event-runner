package com.abstractcode.eventrunner.messaging

import java.net.URI

import cats.data.OptionT
import cats.effect.{Blocker, ContextShift, Resource, Sync}
import cats.implicits._
import com.abstractcode.eventrunner.MessageContainer
import com.abstractcode.eventrunner.MessageProcessor.{MessageContainerWithFinaliser, MessageSource}
import com.abstractcode.eventrunner.messaging.SqsMessageSourceConfiguration.{SqsLocalstack, SqsProduction}
import org.http4s.Uri
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider
import software.amazon.awssdk.services._
import software.amazon.awssdk.services.sqs.SqsClient
import software.amazon.awssdk.services.sqs.model.{DeleteMessageRequest, ReceiveMessageRequest}

import scala.jdk.CollectionConverters._

object SqsMessageSource {
  def clientResource[F[_] : Sync : ContextShift](blocker: Blocker)(configuration: SqsMessageSourceConfiguration): Resource[F, SqsClient] = {
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

    Resource.fromAutoCloseableBlocking(blocker)(buildSqsClient)
  }

  def retrieveMessage[F[_] : Sync : ContextShift, Container <: MessageContainer[_, _]](blocker: Blocker, configuration: SqsMessageSourceConfiguration)(queueUri: Uri, messageParser: sqs.model.Message => F[Container])(sqsClient: SqsClient): MessageSource[F, Container] = {
    val queueUriString = queueUri.renderString

    val receiveRequest = ReceiveMessageRequest.builder()
      .queueUrl(queueUriString)
      .maxNumberOfMessages(1)
      .waitTimeSeconds(configuration.waitTime.value)
      .build()

    val receiveMessage: F[Option[sqs.model.Message]] = blocker.blockOn {
      Sync[F].delay {
        sqsClient.receiveMessage(receiveRequest).messages().asScala.headOption
      }
    }

    def deleteMessage(message: sqs.model.Message): F[Unit] = blocker.blockOn {
      Sync[F].delay(
        sqsClient.deleteMessage(
          DeleteMessageRequest.builder()
            .queueUrl(queueUriString)
            .receiptHandle(message.receiptHandle())
            .build()
        )).map(_ => ())
    }

    () => {
      (for {
        sqsMessage <- OptionT(receiveMessage)
        parsedMessage <- OptionT.liftF(messageParser(sqsMessage))
      } yield MessageContainerWithFinaliser[F, Container](parsedMessage, () => deleteMessage(sqsMessage))).value
    }
  }
}
