package com.abstractcode

import cats.data.Kleisli
import cats.syntax.all._
import cats.{MonadError, Show}
import com.abstractcode.eventrunner.ProcessingError.UnknownHandler
import com.abstractcode.eventrunner.logging.Logged

package object eventrunner {
  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]

  object ThrowableMonadError {
    def apply[F[_]](implicit throwableMonadError: ThrowableMonadError[F]): ThrowableMonadError[F] = throwableMonadError
  }

  type MessageHandler[F[_], Message] = Message => F[Unit]

  type MessageAction[F[_], -MD <: Metadata[_, _], A] = Kleisli[F, MD, A]
  type MessageContext[F[_], -MD <: Metadata[_, _]] = MessageAction[F, MD, Unit]

  def buildMessageHandler[F[_], Message](handler: PartialFunction[Message, F[Unit]], fallback: Message => F[Unit]): MessageHandler[F, Message] =
    message => handler.applyOrElse(message, fallback)

  def loggingFallbackHandler[F[_] : ThrowableMonadError, Message, MT, MD <: Metadata[_, MT]](implicit logged: Logged[F], showType: Show[MT]): Message => Kleisli[F, MD, Unit] =
    _ => Kleisli(metadata =>
      for {
        _ <- logged.log(s"Had no handler for message of type '${metadata.messageType.show}'")
        _ <- ThrowableMonadError[F].raiseError[Unit](UnknownHandler(metadata.messageType))
      } yield ()
    )
}