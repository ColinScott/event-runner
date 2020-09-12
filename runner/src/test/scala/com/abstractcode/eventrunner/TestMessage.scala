package com.abstractcode.eventrunner

import org.scalacheck.{Arbitrary, Gen}

sealed trait TestMessage
case class FirstTestMessage(message: String) extends TestMessage
case class SecondTestMessage(message: String) extends TestMessage
case class TestMetadata(transactionId: String, messageType: String) extends Metadata[String, String]

object TestMessage {
  type TestContainer = MessageContainer[TestMessage, TestMetadata]

  val firstTestMessageGen: Gen[FirstTestMessage] = Gen.identifier.map(FirstTestMessage)
  val secondTestMessageGen: Gen[SecondTestMessage] = Gen.identifier.map(SecondTestMessage)

  val testMessageGen: Gen[TestMessage] = Gen.oneOf(firstTestMessageGen, secondTestMessageGen)

  val testMetadataGen: Gen[TestMetadata] = for {
    transactionId <- Gen.identifier
    messageType <- Gen.identifier
  } yield TestMetadata(transactionId, messageType)

  implicit val arbitraryFirstTestMessage: Arbitrary[FirstTestMessage] = Arbitrary(firstTestMessageGen)
  implicit val arbitraryTestMessage: Arbitrary[TestMessage] = Arbitrary(testMessageGen)
  implicit val arbitraryTestMetadata: Arbitrary[TestMetadata] = Arbitrary(testMetadataGen)

  implicit val arbitraryMessageContainer: Arbitrary[TestContainer] = Arbitrary(
    for {
      testMessage <- testMessageGen
      testMetadata <- testMetadataGen
    } yield MessageContainer(testMessage, testMetadata)
  )
}