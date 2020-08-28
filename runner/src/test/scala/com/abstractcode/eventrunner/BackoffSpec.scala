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
        0 ! 5 ! 60 ! 5 |
        10 ! 5 ! 60 ! 15 |
        60 ! 5 ! 60 ! 60 |
        -10 ! 5 ! 60 ! 0 |
        linearBackoffStoreUpdatedBackoff
    }
            sleep ${
      "initial" | "expected" |>
        0 ! 0 |
        10 ! 10 |
        -10 ! 0 |
        linearShouldSleepForInitialBackoff
    }
            reset $linearCanReset

         exponentialBackoff should
            store updated backoff with maximum ${
      "initial" | "expected" |>
        0 ! 1 |
        1 ! 2 |
        2 ! 3 |
        8 ! 8 |
        -1 ! 0 |
        exponentialBackoffStoreUpdatedBackoff
    }
            sleep ${
      "initial" | "expected" |>
        0 ! 0 |
        2 ! 4 |
        3 ! 9 |
        9 ! 60 |
        -10 ! 0 |
        exponentialShouldSleepForInitialBackoff
    }
            reset $exponentialCanReset
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

  def linearBackoffStoreUpdatedBackoff(initial: Int, increment: Int, maximum: Int, expected: Int): MatchResult[Any] = {
    implicit val timer: Timer[IO] = doNothingTimer

    val result = for {
      ref <- Ref.of[IO, FiniteDuration](FiniteDuration(initial, SECONDS))
      _ <- Backoff.linearBackoff(FiniteDuration(maximum, SECONDS))(FiniteDuration(increment, SECONDS))(ref)(IO.unit)
      value <- ref.get
    } yield value

    result.unsafeRunSync() shouldEqual FiniteDuration(expected, SECONDS)
  }

  def linearShouldSleepForInitialBackoff(initial: Int, expected: Int): MatchResult[Any] = {
    val result = for {
      ref <- Ref.of[IO, FiniteDuration](FiniteDuration(initial, SECONDS))
      sleep <- Ref.of[IO, FiniteDuration](FiniteDuration(-10, DAYS))
      _ <- Backoff.linearBackoff(FiniteDuration(60, SECONDS))(FiniteDuration(5, SECONDS))(ref)(IO.unit)(buildTimerRef(sleep), Sync[IO])
      value <- sleep.get
    } yield value

    result.unsafeRunSync() shouldEqual FiniteDuration(expected, SECONDS)
  }

  def linearCanReset: MatchResult[Any] = {
    val result = for {
      ref <- Ref.of[IO, FiniteDuration](FiniteDuration(5, SECONDS))
      _ <- Backoff.resetLinearBackoff[IO](ref)
      value <- ref.get
    } yield value

    result.unsafeRunSync() shouldEqual Duration.Zero
  }

  def exponentialBackoffStoreUpdatedBackoff(initial: Int, expected: Int): MatchResult[Any] = {
    implicit val timer: Timer[IO] = doNothingTimer

    val result = for {
      ref <- Ref.of[IO, Int](initial)
      _ <- Backoff.exponentialBackoff(FiniteDuration(60, SECONDS))(2.0, FiniteDuration(1, SECONDS))(ref)(IO.unit)
      value <- ref.get
    } yield value

    result.unsafeRunSync() shouldEqual expected
  }

  def exponentialShouldSleepForInitialBackoff(initial: Int, expected: Int): MatchResult[Any] = {
    val result = for {
      ref <- Ref.of[IO, Int](initial)
      sleep <- Ref.of[IO, FiniteDuration](FiniteDuration(-10, DAYS))
      _ <- Backoff.exponentialBackoff(FiniteDuration(60, SECONDS))(2.0, FiniteDuration(1, SECONDS))(ref)(IO.unit)(buildTimerRef(sleep), Sync[IO])
      value <- sleep.get
    } yield value

    result.unsafeRunSync() shouldEqual FiniteDuration(expected, SECONDS)
  }

  def exponentialCanReset: MatchResult[Any] = {
    val result = for {
      ref <- Ref.of[IO, Int](234)
      _ <- Backoff.resetExponentialBackoff[IO](ref)
      value <- ref.get
    } yield value

    result.unsafeRunSync() shouldEqual 0
  }
}
