package com.abstractcode.eventrunner.circe

import cats.data.EitherT
import com.abstractcode.eventrunner.{MessageContainer, MetadataWithType}
import io.circe.{Decoder, DecodingFailure, HCursor}

object MessageContainerDecoder {
  def build[Message, T, Metadata <: MetadataWithType[T]](messageDecoders: T => Option[Decoder[Message]])(metadataDecoder: Decoder[Metadata]): Decoder[MessageContainer[Message, Metadata]] =
    (c: HCursor) => (for {
      metadata <- EitherT.fromEither(metadataDecoder(c))
      messageDecoder <- EitherT.fromOption(messageDecoders(metadata.messageType), DecodingFailure(s"No decoder for message type ${metadata.messageType}", Nil))
      message <- EitherT.fromEither(messageDecoder(c))
    } yield MessageContainer(message, metadata)).value
}
