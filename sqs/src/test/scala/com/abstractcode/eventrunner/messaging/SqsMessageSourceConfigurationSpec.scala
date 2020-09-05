package com.abstractcode.eventrunner.messaging

import cats.syntax.all._
import com.abstractcode.eventrunner.configuration.ParseError.InvalidFormat
import com.abstractcode.eventrunner.messaging.SqsMessageSourceConfiguration.{SqsProduction, WaitTime}
import eu.timepit.refined.scalacheck.all._
import org.scalacheck.Gen
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction1
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class SqsMessageSourceConfigurationSpec extends Specification with ScalaCheck {
  def is: SpecStructure =
    s2"""
         SqsMessageSourceConfiguration should
            parse from environment variables for production $shouldParseForProduction
            not parse with out of bounds wait time $shouldNotParseOutOfBoundsWaitTime
      """

  def shouldParseForProduction: ScalaCheckFunction1[WaitTime, MatchResult[Any]] =
    prop {
      (waitTime: WaitTime) =>
        val environment: Map[String, String] = Map("QUEUE_WAIT_TIME" -> waitTime.toString)

        val expected = SqsMessageSourceConfiguration(SqsProduction, waitTime)

        val result = SqsMessageSourceConfiguration.parse(environment)

        result shouldEqual expected.validNec
    }

  def shouldNotParseOutOfBoundsWaitTime: ScalaCheckFunction1[Int, MatchResult[Any]] =
    prop {
      (waitTime: Int) =>
        val environment: Map[String, String] = Map("QUEUE_WAIT_TIME" -> waitTime.toString)

        val expected = InvalidFormat("QUEUE_WAIT_TIME").invalidNec

        val result = SqsMessageSourceConfiguration.parse(environment)

        result shouldEqual expected
    }.setGen(Gen.chooseNum(Int.MinValue, Int.MaxValue).filter(i => i < 0 || i > 20))
}
