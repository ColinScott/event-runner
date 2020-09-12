package com.abstractcode.eventrunner

import cats.data.Kleisli
import cats.{Applicative, Monad}
import cats.syntax.all._
import com.abstractcode.eventrunner.MessageProcessor.{MessageContainerWithFinaliser, MessageSource}

trait Metadata[Transaction, MT] {
  val transactionId: Transaction
  val messageType: MT
}

case class MessageContainer[+Message, MD <: Metadata[_, _]](message: Message, metadata: MD)

class MessageProcessor[F[_] : Monad, +Message, MD <: Metadata[_, _]](source: MessageSource[F, MessageContainer[Message, MD]], handler: MessageHandler[Kleisli[F, MD, *], Message]) {
  def process(): F[Unit] = {
    def processContainer(containerWithFinaliser: MessageContainerWithFinaliser[F, MessageContainer[Message, MD]]): F[Unit] = {
      for {
        _ <- handler(containerWithFinaliser.container.message).run(containerWithFinaliser.container.metadata)
        _ <- containerWithFinaliser.finalise()
      } yield ()
    }

    for {
      container <- source()
      _ <- container.fold(Applicative[F].pure(()))(processContainer)
    } yield ()
  }
}

object MessageProcessor {
  type MessageSource[F[_], Container <: MessageContainer[_, _]] = () => F[Option[MessageContainerWithFinaliser[F, Container]]]

  case class MessageContainerWithFinaliser[F[_], Container <: MessageContainer[_, _]](container: Container, finalise: () => F[Unit])
}
