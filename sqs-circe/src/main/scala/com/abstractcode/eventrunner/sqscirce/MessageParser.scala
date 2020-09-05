package com.abstractcode.eventrunner.sqscirce

import cats.implicits._
import com.abstractcode.eventrunner.{MessageContainer, ThrowableMonadError}
import io.circe.{Decoder, parser}
import software.amazon.awssdk.services.sqs

object MessageParser {
  def build[F[_]: ThrowableMonadError, Container <: MessageContainer[_, _]](implicit containerDecoder: Decoder[Container]): sqs.model.Message => F[Container] = sqsMessage => for {
    parsed <- ThrowableMonadError[F].fromEither(parser.decode[Container](sqsMessage.body()))
  } yield parsed
}
