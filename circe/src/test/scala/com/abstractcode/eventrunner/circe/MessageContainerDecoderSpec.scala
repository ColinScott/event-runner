package com.abstractcode.eventrunner.circe

import java.util.UUID

import com.abstractcode.eventrunner.{MessageContainer, Metadata}
import io.circe
import io.circe.{Decoder, DecodingFailure, HCursor, parser}
import org.scalacheck.Gen
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.{ScalaCheckFunction2, ScalaCheckFunction3}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class MessageContainerDecoderSpec extends Specification with ScalaCheck {
  type TransactionId = UUID
  type MessageType = String

  case class TestMetadata(transactionId: TransactionId, messageType: MessageType) extends Metadata[TransactionId, MessageType]

  sealed trait TestMessage
  case class FirstTestMessage(count: Int) extends TestMessage

  type TestContainer = MessageContainer[TestMessage, TestMetadata]

  val metadataDecoder: Decoder[TestMetadata] = (c: HCursor) => for {
    transactionId <- c.downField("transactionId").as[TransactionId]
    messageType <- c.downField("messageType").as[MessageType]
  } yield TestMetadata(transactionId, messageType)

  val firstMessageDecoder: Decoder[TestMessage] = (c: HCursor) => for {
    count <- c.downField("count").as[Int]
  } yield FirstTestMessage(count)

  def is: SpecStructure =
    s2"""
         MessageContainerDecoder should
            decode message with known type $decodeWithKnownType
            error on unknown type $errorOnUnknownType
            error on missing type $errorOnMissingType
            error on missing transaction ID $errorOnMissingTransactionId
            error on decode failure $errorOnDecodeFailure
    """

  def decodeWithKnownType: ScalaCheckFunction3[TransactionId, MessageType, Int, MatchResult[Either[circe.Error, TestMessage]]] =
    prop { (transactionId: TransactionId, messageType: MessageType, count: Int) =>

      val messageDecoders: MessageType => Option[Decoder[TestMessage]] = {
        case `messageType` => Some(firstMessageDecoder)
        case _ => None
      }

      implicit val decoder: Decoder[TestContainer] = MessageContainerDecoder.build(messageDecoders)(metadataDecoder)

      val messageJson =
        s"""{
           | "transactionId": "$transactionId",
           | "messageType": "$messageType",
           | "count": $count
           |}""".stripMargin

      val result = parser.decode[TestContainer](messageJson)

      result.map(_.message) should beRight(FirstTestMessage(count))
    }.setGen2(Gen.alphaStr)

  def errorOnUnknownType: ScalaCheckFunction3[TransactionId, MessageType, Int, MatchResult[Either[circe.Error, TestContainer]]] =
    prop {
      (transactionId: TransactionId, messageType: MessageType, count: Int) =>
        val messageDecoders: MessageType => Option[Decoder[TestMessage]] = _ => None

        implicit val decoder: Decoder[TestContainer] = MessageContainerDecoder.build(messageDecoders)(metadataDecoder)

        val messageJson =
          s"""{
             | "transactionId": "$transactionId",
             | "messageType": "$messageType",
             | "count": $count
             |}""".stripMargin

        val result = parser.decode[TestContainer](messageJson)

        result should beLeft(DecodingFailure(s"No decoder for message type $messageType", Nil))
    }.setGen2(Gen.alphaStr)

  def errorOnMissingType: ScalaCheckFunction2[TransactionId, Int, MatchResult[Either[circe.Error, TestContainer]]] =
    prop {
      (transactionId: TransactionId, count: Int) =>
        val messageDecoders: MessageType => Option[Decoder[TestMessage]] = _ => None

        implicit val decoder: Decoder[TestContainer] = MessageContainerDecoder.build(messageDecoders)(metadataDecoder)

        val messageJson =
          s"""{
             | "transactionId": "$transactionId",
             | "count": $count
             |}""".stripMargin

        val result = parser.decode[TestContainer](messageJson)

        result should beLeft()
    }

  def errorOnMissingTransactionId: ScalaCheckFunction2[TransactionId, Int, MatchResult[Either[circe.Error, TestContainer]]] =
    prop {
      (transactionId: TransactionId, count: Int) =>
        val messageDecoders: MessageType => Option[Decoder[TestMessage]] = _ => None

        implicit val decoder: Decoder[TestContainer] = MessageContainerDecoder.build(messageDecoders)(metadataDecoder)

        val messageJson =
          s"""{
             | "transactionId": "$transactionId",
             | "count": $count
             |}""".stripMargin

        val result = parser.decode[TestContainer](messageJson)

        result should beLeft()
    }

  def errorOnDecodeFailure: ScalaCheckFunction2[TransactionId, MessageType, MatchResult[Either[circe.Error, TestContainer]]] =
    prop {
      (transactionId: TransactionId, messageType: MessageType) =>
        val messageDecoders: MessageType => Option[Decoder[TestMessage]] = {
          case `messageType` => Some(firstMessageDecoder)
          case _ => None
        }

        implicit val decoder: Decoder[TestContainer] = MessageContainerDecoder.build(messageDecoders)(metadataDecoder)

        val messageJson =
          s"""{
             | "transactionId": "$transactionId",
             | "messageType": "$messageType"
             |}""".stripMargin

        val result = parser.decode[TestContainer](messageJson)

        result should beLeft()
    }.setGen2(Gen.alphaStr)
}
