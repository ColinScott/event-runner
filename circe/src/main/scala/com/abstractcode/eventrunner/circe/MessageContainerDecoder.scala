package com.abstractcode.eventrunner.circe

import com.abstractcode.eventrunner.{MessageContainer, Metadata}
import io.circe.{Decoder, DecodingFailure, HCursor}

object MessageContainerDecoder {
  def build[Message, MT, MD <: Metadata[_, MT]](messageDecoders: MT => Option[Decoder[Message]])(metadataDecoder: Decoder[MD]): Decoder[MessageContainer[Message, MD]] =
    (c: HCursor) => for {
      metadata <- metadataDecoder(c)
      messageDecoder <- messageDecoders(metadata.messageType).toRight(DecodingFailure(s"No decoder for message type ${metadata.messageType}", Nil))
      message <- messageDecoder(c)
    } yield MessageContainer(message, metadata)
}
