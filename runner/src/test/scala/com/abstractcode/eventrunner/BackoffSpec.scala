package com.abstractcode.eventrunner

import cats.effect.concurrent.Ref
import cats.effect.{Clock, IO, Sync, Timer}
import org.specs2.Specification
import org.specs2.matcher.MatchResult
import org.specs2.specification.Tables
import org.specs2.specification.core.SpecStructure

import scala.concurrent.duration.{DAYS, Duration, FiniteDuration, SECONDS}

class BackoffSpec extends Specification with Tables {
  def is: SpecStructure =
    s2"""
         linearBackoff should
            store updated backoff ${
      "initial" | "increment" | "maximum" | "expected" |>
        FiniteDuration(0, SECONDS) ! FiniteDuration(5, SECONDS) ! FiniteDuration(60, SECONDS) ! FiniteDuration(5, SECONDS) |
        FiniteDuration(10, SECONDS) ! FiniteDuration(5, SECONDS) ! FiniteDuration(60, SECONDS) ! FiniteDuration(15, SECONDS) |
        FiniteDuration(60, SECONDS) ! FiniteDuration(5, SECONDS) ! FiniteDuration(60, SECONDS) ! FiniteDuration(60, SECONDS) |
        FiniteDuration(-10, SECONDS) ! FiniteDuration(5, SECONDS) ! FiniteDuration(60, SECONDS) ! FiniteDuration(0, SECONDS) | {
        (initial, increment, maximum, expected) => linearBackoffStoreUpdatedBackoff(initial, increment, maximum, expected)
      }
    }
            sleep ${
      "initial" | "expected" |>
        FiniteDuration(0, SECONDS) ! FiniteDuration(0, SECONDS) |
        FiniteDuration(10, SECONDS) ! FiniteDuration(10, SECONDS) |
        FiniteDuration(-10, SECONDS) ! FiniteDuration(0, SECONDS) | {
        (initial, expected) => shouldSleepForInitialBackoff(initial, expected)
      }
    }
          Can reset $shouldReset
      """

  def linearBackoffStoreUpdatedBackoff(initial: FiniteDuration, increment: FiniteDuration, maximum: FiniteDuration, expected: FiniteDuration): MatchResult[Any] = {
    implicit val timer: Timer[IO] = new Timer[IO] {
      def clock: Clock[IO] = throw new NotImplementedError()

      def sleep(duration: FiniteDuration): IO[Unit] = IO.unit
    }

    val result = for {
      ref <- Ref.of[IO, FiniteDuration](initial)
      _ <- Backoff.linearBackoff(maximum)(increment)(ref)(IO.unit)
      value <- ref.get
    } yield value

    result.unsafeRunSync() shouldEqual expected
  }

  def shouldSleepForInitialBackoff(initial: FiniteDuration, expected: FiniteDuration): MatchResult[Any] = {
    val timer: Ref[IO, FiniteDuration] => Timer[IO] = s => new Timer[IO] {
      def clock: Clock[IO] = throw new NotImplementedError()
      def sleep(duration: FiniteDuration): IO[Unit] =  s.set(duration)
    }

    val result = for {
      ref <- Ref.of[IO, FiniteDuration](initial)
      sleep <- Ref.of[IO, FiniteDuration](FiniteDuration(-10, DAYS))
      _ <- Backoff.linearBackoff(FiniteDuration(60, SECONDS))(FiniteDuration(5, SECONDS))(ref)(IO.unit)(timer(sleep), Sync[IO])
      value <- sleep.get
    } yield value

    result.unsafeRunSync() shouldEqual expected
  }

  def shouldReset: MatchResult[Any] = {
    val result = for {
      ref <- Ref.of[IO, FiniteDuration](FiniteDuration(5, SECONDS))
      _ <- Backoff.resetBackoff[IO, Unit](ref)
      value <- ref.get
    } yield value

    result.unsafeRunSync() shouldEqual Duration.Zero
  }
}
