package com.abstractcode.eventrunner

import cats.{Applicative, Monad}
import cats.implicits._
import com.abstractcode.eventrunner.MessageProcessor.MessageHandler

case class MessageContainer[F[_], Message](message: Message, finalise: () => F[Unit])

class MessageProcessor[F[_] : Monad, Message](source: () => F[Option[MessageContainer[F, Message]]], handler: MessageHandler[F, Message]) {
  def process(): F[Unit] = {
    def processContainer(container: MessageContainer[F, Message]): F[Unit] = for {
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
  type MessageHandler[F[_], Message] = PartialFunction[Message, F[Unit]]
}
