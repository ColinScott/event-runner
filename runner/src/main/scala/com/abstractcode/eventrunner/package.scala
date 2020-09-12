package com.abstractcode

import cats.data.Kleisli
import cats.effect.Clock
import cats.syntax.all._
import cats.{MonadError, Show}
import com.abstractcode.eventrunner.MessageProcessor.MessageSource
import com.abstractcode.eventrunner.ProcessingError.UnknownHandler
import com.abstractcode.eventrunner.logging.{CirceLogged, Logged}
import fs2.concurrent.Queue
import io.circe.{Encoder, Json}

package object eventrunner {
  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]

  object ThrowableMonadError {
    def apply[F[_]](implicit throwableMonadError: ThrowableMonadError[F]): ThrowableMonadError[F] = throwableMonadError
  }

  type MessageHandler[F[_], Message] = Message => F[Unit]

  type MessageContext[F[_], -MD <: Metadata[_, _]] = Kleisli[F, MD, Unit]

  def buildMessageHandler[F[_], Message](handler: PartialFunction[Message, F[Unit]], fallback: Message => F[Unit]): MessageHandler[F, Message] =
    message => handler.applyOrElse(message, fallback)

  def loggingFallbackHandler[F[_] : ThrowableMonadError, Message, MT, MD <: Metadata[_, MT]](implicit logged: Logged[F], showType: Show[MT]): Message => Kleisli[F, MD, Unit] =
    _ => Kleisli(metadata =>
      for {
        _ <- logged.log(s"Had no handler for message of type '${metadata.messageType.show}'")
        _ <- ThrowableMonadError[F].raiseError[Unit](UnknownHandler(metadata.messageType))
      } yield ()
    )

  def buildMessageProcessor[F[_] : ThrowableMonadError : Clock : Logged, MD <: Metadata[Transaction, MessageType], Transaction, MessageType, Message, Context](
    logQueue: Queue[F, Json],
    retrieveWithClient: Context => MessageSource[F, MessageContainer[Message, MD]],
    handlers: Logged[Kleisli[F, MD, *]] => PartialFunction[Message, Kleisli[F, MD, Unit]]
  )(
    implicit transactionIdEncoder: Encoder[Transaction], showType: Show[MessageType]
  ): Context => MessageProcessor[F, Message, MD] = {
    val logged: CirceLogged[F, Transaction, MD] = new CirceLogged[F, Transaction, MD](logQueue)

    val messageHandler = buildMessageHandler[Kleisli[F, MD, *], Message](handlers(logged), loggingFallbackHandler[F, Message, MessageType, MD])

    (context: Context) => new MessageProcessor[F, Message, MD](retrieveWithClient(context), messageHandler)
  }
}