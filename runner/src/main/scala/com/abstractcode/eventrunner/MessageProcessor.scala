package com.abstractcode.eventrunner

import cats.{Applicative, Monad}
import cats.implicits._
import com.abstractcode.eventrunner.MessageProcessor.{MCF, MessageSource}

trait Metadata[T, MT] {
  val transactionId: T
  val messageType: MT
}

case class MessageContainer[Message, MD <: Metadata[_, _]](message: Message, metadata: MD)

class MessageProcessor[F[_] : Monad, Container <: MessageContainer[_, _]](source: MessageSource[F, Container], handler: MessageHandler[F, Container]) {
  def process(): F[Unit] = {
    def processContainer(containerWithFinaliser: MCF[F,Container]): F[Unit] = {
      for {
        _ <- handler(containerWithFinaliser.container)
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
  type MessageSource[F[_], Container <: MessageContainer[_, _]] = () => F[Option[MCF[F, Container]]]

  case class MCF[F[_], Container <: MessageContainer[_, _]](container: Container, finalise: () => F[Unit])
}
