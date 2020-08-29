package com.abstractcode.eventrunner.circe

import cats.data.EitherT
import com.abstractcode.eventrunner.{MessageContainer, Metadata}
import io.circe.{Decoder, DecodingFailure, HCursor}

object MessageContainerDecoder {
  def build[Message, MT, MD <: Metadata[_, MT]](messageDecoders: MT => Option[Decoder[Message]])(metadataDecoder: Decoder[MD]): Decoder[MessageContainer[Message, MD]] =
    (c: HCursor) => (for {
      metadata <- EitherT.fromEither(metadataDecoder(c))
      messageDecoder <- EitherT.fromOption(messageDecoders(metadata.messageType), DecodingFailure(s"No decoder for message type ${metadata.messageType}", Nil))
      message <- EitherT.fromEither(messageDecoder(c))
    } yield MessageContainer(message, metadata)).value
}
