package com.abstractcode

import cats.data.Kleisli
import cats.implicits._
import cats.{Applicative, MonadError, Show}
import com.abstractcode.eventrunner.ProcessingError.UnknownHandler
import com.abstractcode.eventrunner.logging.Logged

package object eventrunner {
  type ThrowableMonadError[F[_]] = MonadError[F, Throwable]

  object ThrowableMonadError {
    def apply[F[_]](implicit throwableMonadError: ThrowableMonadError[F]): ThrowableMonadError[F] = throwableMonadError
  }

  type ContainerHandler[F[_], Container <: MessageContainer[_, _]] = Container => F[Unit]

  type MessageContext[F[_], -MD <: Metadata[_, _]] = Kleisli[F, MD, Unit]

  def buildMessageHandler[F[_], M, MD <: Metadata[_, _]](handler: PartialFunction[M, MessageContext[F, MD]], fallback: M => MessageContext[F, MD]): ContainerHandler[F, MessageContainer[M, MD]] =
    container => handler.applyOrElse(container.message, fallback).run(container.metadata)

  def loggingFallbackHandler[F[_]: ThrowableMonadError, M, MT, MD <: Metadata[_, MT]](implicit logged: Logged[F, MD], showType: Show[MT]): M => MessageContext[F, MD] =
    _ => for {
        metadata <- Kleisli[F, MD, MD](m => Applicative[F].pure(m))
        _ <- logged.log(s"Had no handler for message of type '${metadata.messageType.show}'")
        _ <- Kleisli.liftF(ThrowableMonadError[F].raiseError[Unit](UnknownHandler(metadata.messageType)))
      } yield ()
}
