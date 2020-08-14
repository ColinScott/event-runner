package com.abstractcode.eventrunner.messaging

import cats.implicits._
import com.abstractcode.eventrunner._
import com.abstractcode.eventrunner.messaging.ParseError.{InvalidFormat, NotProvidedOrEmpty}
import com.abstractcode.eventrunner.messaging.SqsMessageSourceConfiguration.{SqsProduction, WaitTime}
import eu.timepit.refined.scalacheck.all._
import org.http4s.Uri
import org.scalacheck.Gen
import org.scalacheck.Gen.const
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction2
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class SqsMessageSourceConfigurationSpec extends Specification with ScalaCheck {
  def is: SpecStructure =
    s2"""
         SqsMessageSourceConfiguration should
            parse from environment variables for production $shouldParseForProduction
            not parse with missing queue URI $shouldNotParseMissingQueueUri
            not parse with out of bounds wait time $shouldNotParseOutOfBoundsWaitTime
      """

  def shouldParseForProduction: ScalaCheckFunction2[Uri, WaitTime, MatchResult[Any]] =
    prop {
      (uri: Uri, waitTime: WaitTime) =>
        val environment: Map[String, String] = Map("QUEUE_URL" -> uri.renderString, "QUEUE_WAIT_TIME" -> waitTime.toString)

        val expected = SqsMessageSourceConfiguration(uri, SqsProduction, waitTime)

        val result = SqsMessageSourceConfiguration(environment)

        result shouldEqual expected.validNec
    }

  def shouldNotParseMissingQueueUri: ScalaCheckFunction2[String, WaitTime, MatchResult[Any]] =
    prop {
      (badUri: String, waitTime: WaitTime) =>
        val environment: Map[String, String] = Map("QUEUE_URL" -> badUri, "QUEUE_WAIT_TIME" -> waitTime.toString)

        val expected = ParseError("QUEUE_URL", NotProvidedOrEmpty).invalidNec

        val result = SqsMessageSourceConfiguration(environment)

        result shouldEqual expected
    }.setGen1(Gen.listOf(" ").map(_.mkString))

  def shouldNotParseOutOfBoundsWaitTime: ScalaCheckFunction2[Uri, Int, MatchResult[Any]] =
    prop {
      (uri: Uri, waitTime: Int) =>
        val environment: Map[String, String] = Map("QUEUE_URL" -> uri.renderString, "QUEUE_WAIT_TIME" -> waitTime.toString)

        val expected = ParseError("QUEUE_WAIT_TIME", InvalidFormat).invalidNec

        val result = SqsMessageSourceConfiguration(environment)

        result shouldEqual expected
    }.setGen2(Gen.chooseNum(Int.MinValue, Int.MaxValue).filter(i => i < 0 || i > 20))
}
