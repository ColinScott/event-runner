package com.abstractcode.eventrunner

import cats.implicits._
import cats.{Applicative, Monad}
import com.abstractcode.eventrunner.MessageProcessor.{MCF, MessageSource}

trait MetadataWithType[T] {
  val messageType: T
}

case class MessageContainer[Message, Metadata <: MetadataWithType[_]](message: Message, metadata: Metadata)

class MessageProcessor[F[_] : Monad, Container <: MessageContainer[_, _]](source: MessageSource[F, Container], handler: MessageHandler[F, Container]) {
  def process(): F[Unit] = {
    def processContainer(containerWithFinaliser: MCF[F,Container]): F[Unit] = for {
      _ <- handler(containerWithFinaliser.container)
      _ <- containerWithFinaliser.finalise()
    } yield ()

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
