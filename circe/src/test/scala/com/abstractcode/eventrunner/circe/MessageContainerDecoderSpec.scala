package com.abstractcode.eventrunner.circe

import com.abstractcode.eventrunner.{MessageContainer, MetadataWithType}
import io.circe
import io.circe.{Decoder, DecodingFailure, HCursor, parser}
import org.scalacheck.Gen
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.{ScalaCheckFunction1, ScalaCheckFunction2}
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class MessageContainerDecoderSpec extends Specification with ScalaCheck {
  case class TestMetadata(messageType: String) extends MetadataWithType[String]

  sealed trait TestMessage
  case class FirstTestMessage(count: Int) extends TestMessage

  type TestContainer = MessageContainer[TestMessage, TestMetadata]

  val metadataDecoder: Decoder[TestMetadata] = (c: HCursor) => for {
    messageType <- c.downField("messageType").as[String]
  } yield TestMetadata(messageType)

  val firstMessageDecoder: Decoder[TestMessage] = (c: HCursor) => for {
    count <- c.downField("count").as[Int]
  } yield FirstTestMessage(count)

  def is: SpecStructure =
    s2"""
         MessageContainerDecoder should
            decode message with known type $decodeWithKnownType
            error with unknown type $errorWithUnknownType
            error on decode failure $errorOnDecodeFailure
    """

  def decodeWithKnownType: ScalaCheckFunction2[String, Int, MatchResult[Either[circe.Error, TestMessage]]] =
    prop { (messageType: String, count: Int) =>

      val messageDecoders: String => Option[Decoder[TestMessage]] = {
        case `messageType` => Some(firstMessageDecoder)
        case _ => None
      }

      implicit val decoder: Decoder[TestContainer] = MessageContainerDecoder.build(messageDecoders)(metadataDecoder)

      val messageJson =
        s"""{
           | "messageType": "$messageType",
           | "count": $count
           |}""".stripMargin

      val result = parser.decode[TestContainer](messageJson)

      result.map(_.message) should beRight(FirstTestMessage(count))
    }.setGen1(Gen.alphaStr)

  def errorWithUnknownType: ScalaCheckFunction2[String, Int, MatchResult[Either[circe.Error, TestContainer]]] =
    prop {
      (messageType: String, count: Int) =>
        val messageDecoders: String => Option[Decoder[TestMessage]] = _ => None

        implicit val decoder: Decoder[TestContainer] = MessageContainerDecoder.build(messageDecoders)(metadataDecoder)

        val messageJson =
          s"""{
             | "messageType": "$messageType",
             | "count": $count
             |}""".stripMargin

        val result = parser.decode[TestContainer](messageJson)

        result should beLeft(DecodingFailure(s"No decoder for message type ${messageType}", Nil))
    }.setGen1(Gen.alphaStr)

  def errorOnDecodeFailure: ScalaCheckFunction1[String, MatchResult[Either[circe.Error, TestContainer]]] =
    prop {
      (messageType: String) =>
        val messageDecoders: String => Option[Decoder[TestMessage]] = {
          case `messageType` => Some(firstMessageDecoder)
          case _ => None
        }

        implicit val decoder: Decoder[TestContainer] = MessageContainerDecoder.build(messageDecoders)(metadataDecoder)

        val messageJson =
          s"""{
             | "messageType": "$messageType"
             |}""".stripMargin

        val result = parser.decode[TestContainer](messageJson)

        result should beLeft()
    }.setGen(Gen.alphaStr)
}
