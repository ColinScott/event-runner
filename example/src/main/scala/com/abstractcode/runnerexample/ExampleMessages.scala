package com.abstractcode.runnerexample

import java.util.UUID

import com.abstractcode.eventrunner.circe.MessageContainerDecoder
import com.abstractcode.eventrunner.{MessageContainer, Metadata}
import io.circe.{Decoder, HCursor}


object ExampleMessages {
  sealed trait ExampleMessage
  case class FirstExampleMessage(count: Int) extends ExampleMessage
  case class SecondExampleMessage(count: Int) extends ExampleMessage

  case class ExampleMetadata(transactionId: TransactionId, messageType: MessageType) extends Metadata[TransactionId, MessageType]

  type TransactionId = UUID
  type MessageType = String
  type ExampleContainer = MessageContainer[ExampleMessage, ExampleMetadata]

  val metadataDecoder: Decoder[ExampleMetadata] = (c: HCursor) => {

    val metadata = c.downField("metadata")

    for {
      transactionId <- metadata.downField("transactionId").as[TransactionId]
      messageType <- metadata.downField("messageType").as[MessageType]
    } yield ExampleMetadata(transactionId, messageType)
  }

  val exampleMessageDecoder: Decoder[ExampleMessage] = (c: HCursor) => for {
    count <- c.downField("count").as[Int]
  } yield SecondExampleMessage(count)

  val messageDecoders: MessageType => Option[Decoder[ExampleMessage]] = {
    case "test" => Some(exampleMessageDecoder)
    case _ => None
  }

  implicit val containerDecoder: Decoder[ExampleContainer] = MessageContainerDecoder.build(messageDecoders)(metadataDecoder)
}