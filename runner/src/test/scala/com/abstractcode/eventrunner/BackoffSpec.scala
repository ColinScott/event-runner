package com.abstractcode.eventrunner

import cats.effect.concurrent.Ref
import cats.effect.{Clock, IO, Sync, Timer}
import org.scalacheck.Prop
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.{ScalaCheckFunction1, ScalaCheckFunction2}
import org.specs2.specification.Tables
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

import scala.concurrent.duration.{DAYS, Duration, FiniteDuration, SECONDS}

class BackoffSpec extends Specification with ScalaCheck with Tables {
  def is: SpecStructure =
    s2"""
         backoff should
            return action result $returnActionResult
            sleep for backoff function response $sleepForBackoffFunctionResponse
            store new backoff value $storeNewBackoffValue

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

  val doNothingTimer: Timer[IO] = new Timer[IO] {
    def clock: Clock[IO] = throw new NotImplementedError()

    def sleep(duration: FiniteDuration): IO[Unit] = IO.unit
  }

  val buildTimerRef: Ref[IO, FiniteDuration] => Timer[IO] = s => new Timer[IO] {
    def clock: Clock[IO] = throw new NotImplementedError()
    def sleep(duration: FiniteDuration): IO[Unit] =  s.set(duration)
  }

  def returnActionResult: ScalaCheckFunction1[Int, MatchResult[Any]] =
    prop {
      (expected: Int) =>
        implicit val timer: Timer[IO] = doNothingTimer

        val result = for {
          ref <- Ref.of[IO, Unit](())
          r <- Backoff.backoff[IO, Int, Unit](_ => ((), Duration.Zero))(ref)(IO.pure(expected))
        } yield r

        result.unsafeRunSync() shouldEqual expected
    }

  def sleepForBackoffFunctionResponse: ScalaCheckFunction1[FiniteDuration, MatchResult[Any]] =
    prop {
      (sleepDuration: FiniteDuration) =>
        val result = for {
          ref <- Ref.of[IO, Unit](())
          sleep <- Ref.of[IO, FiniteDuration](FiniteDuration(-10, DAYS))
          _ <- Backoff.backoff((_: Unit) => ((), sleepDuration))(ref)(IO.unit)(buildTimerRef(sleep), Sync[IO])
          value <- sleep.get
        } yield value

        result.unsafeRunSync() shouldEqual sleepDuration
    }

  def storeNewBackoffValue: ScalaCheckFunction2[Int, Int, Prop] =
    prop {
      (initial: Int, expected: Int) =>
        (initial != expected) ==> {
          implicit val timer: Timer[IO] = doNothingTimer

          val result = for {
            ref <- Ref.of[IO, Int](initial)
            _ <- Backoff.backoff[IO, Unit, Int] {
              case `initial` => (expected, Duration.Zero)
              case _ => (initial, Duration.Zero)
            }(ref)(IO.pure(()))
            value <- ref.get
          } yield value

          result.unsafeRunSync() shouldEqual expected
        }
    }

  def linearBackoffStoreUpdatedBackoff(initial: FiniteDuration, increment: FiniteDuration, maximum: FiniteDuration, expected: FiniteDuration): MatchResult[Any] = {
    implicit val timer: Timer[IO] = doNothingTimer

    val result = for {
      ref <- Ref.of[IO, FiniteDuration](initial)
      _ <- Backoff.linearBackoff(maximum)(increment)(ref)(IO.unit)
      value <- ref.get
    } yield value

    result.unsafeRunSync() shouldEqual expected
  }

  def shouldSleepForInitialBackoff(initial: FiniteDuration, expected: FiniteDuration): MatchResult[Any] = {
    val result = for {
      ref <- Ref.of[IO, FiniteDuration](initial)
      sleep <- Ref.of[IO, FiniteDuration](FiniteDuration(-10, DAYS))
      _ <- Backoff.linearBackoff(FiniteDuration(60, SECONDS))(FiniteDuration(5, SECONDS))(ref)(IO.unit)(buildTimerRef(sleep), Sync[IO])
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
