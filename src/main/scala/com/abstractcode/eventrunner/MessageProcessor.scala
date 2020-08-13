package com.abstractcode.eventrunner

import cats.Monad
import cats.implicits._
import com.abstractcode.eventrunner.MessageProcessor.MessageHandler

trait Message
case class MessageContainer[F[_]](message: Message, finalise: () => F[Unit])

class MessageProcessor[F[_] : Monad](source: () => F[MessageContainer[F]], handler: MessageHandler[F]) {
  def process(): F[Unit] = for {
    container <- source()
    _ <- handler(container.message)
    _ <- container.finalise()
  } yield ()
}

object MessageProcessor {
  type MessageHandler[F[_]] = PartialFunction[Message, F[Unit]]
}
