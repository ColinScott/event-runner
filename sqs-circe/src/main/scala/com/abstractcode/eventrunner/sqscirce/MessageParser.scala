package com.abstractcode.eventrunner.sqscirce

import cats.implicits._
import cats.MonadError
import com.abstractcode.eventrunner.MessageContainer
import io.circe.{Decoder, parser}
import software.amazon.awssdk.services.sqs

object MessageParser {
  def build[F[_], Container <: MessageContainer[_, _]](implicit containerDecoder: Decoder[Container], F: MonadError[F, Throwable]): sqs.model.Message => F[Container] = sqsMessage => for {
    parsed <- F.fromEither(parser.decode[Container](sqsMessage.body()))
  } yield parsed
}
