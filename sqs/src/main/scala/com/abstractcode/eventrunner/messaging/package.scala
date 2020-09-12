package com.abstractcode.eventrunner

import cats.effect.concurrent.Ref
import cats.effect.{Blocker, Clock, ContextShift, Sync, Timer}
import com.abstractcode.eventrunner.MessageProcessor.MessageSource
import com.abstractcode.eventrunner.logging.Logged
import io.circe.Encoder
import org.http4s.Uri
import software.amazon.awssdk.services.sqs
import software.amazon.awssdk.services.sqs.SqsClient
import cats.syntax.all._

import scala.concurrent.duration.{FiniteDuration, SECONDS}

package object messaging {
  def retrieveFromSqs[F[_] : Sync : ContextShift, MD <: Metadata[_, _], Message](
    blocker: Blocker, sqsMessageSourceConfiguration: SqsMessageSourceConfiguration, queueUri: Uri, messageParser: sqs.model.Message => F[MessageContainer[Message, MD]]
  ): SqsClient => MessageSource[F, MessageContainer[Message, MD]] =
    (sqsClient: SqsClient) => SqsMessageSource.retrieveMessage[F, MessageContainer[Message, MD]](blocker, sqsMessageSourceConfiguration)(queueUri, messageParser)(sqsClient)

  def runQueue[F[_] : Sync : ContextShift : Clock : Timer : Logged, MD <: Metadata[Transaction, MessageType], Transaction, MessageType, Message](
    blocker: Blocker,
    sqsMessageSourceConfiguration: SqsMessageSourceConfiguration,
    messageProcessor: SqsClient => MessageProcessor[F, Message, MD]
  )(implicit throwableEncoder: Encoder[Throwable]): F[Unit] = {
    for {
      errorBackoff <- Ref.of[F, Int](0)
      backoff = Backoff.exponentialBackoff[F, Unit](FiniteDuration(30, SECONDS))(1.5, FiniteDuration(1, SECONDS))(errorBackoff) _
      _ <- SqsMessageSource.clientResource[F](blocker)(sqsMessageSourceConfiguration).use {
        sqsClient => {
          val processor: MessageProcessor[F, Message, MD] = messageProcessor(sqsClient)
          for {
            _ <- Repeater.repeat(processor.process().flatMap(_ => Backoff.resetExponentialBackoff[F](errorBackoff))
              .handleErrorWith(t => backoff(Logged[F].log(t))))
          } yield ()
        }
      }
    } yield ()
  }

}
