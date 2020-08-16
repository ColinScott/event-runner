package com.abstractcode.eventrunner

import cats.{Applicative, Monad}
import cats.implicits._
import com.abstractcode.eventrunner.MessageProcessor.MessageHandler

trait Message

case class MessageContainer[F[_]](message: Message, finalise: () => F[Unit])

class MessageProcessor[F[_] : Monad](source: () => F[Option[MessageContainer[F]]], handler: MessageHandler[F]) {
  def process(): F[Unit] = {
    def processContainer(container: MessageContainer[F]): F[Unit] = for {
      _ <- handler(container.message)
      _ <- container.finalise()
    } yield ()

    for {
      container <- source()
      _ <- container.fold(Applicative[F].pure(()))(processContainer)
    } yield ()
  }
}

object MessageProcessor {
  type MessageHandler[F[_]] = PartialFunction[Message, F[Unit]]
}
