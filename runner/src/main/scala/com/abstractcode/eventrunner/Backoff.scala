package com.abstractcode.eventrunner

import cats.Monad
import cats.effect.Timer
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.concurrent.duration.{Duration, FiniteDuration}

object Backoff {
  type BackoffAlgorithm[A] = A => (A, FiniteDuration)

  def linearBackoff[F[_]: Timer: Monad, A](maximum: FiniteDuration)(increment: FiniteDuration)(backoffRef: Ref[F, FiniteDuration])(action: F[A]): F[A] = {
    def linearBackoffAlgorithm(maximum: FiniteDuration)(increment: FiniteDuration): BackoffAlgorithm[FiniteDuration] = current => {
      if (current < Duration.Zero) (Duration.Zero, Duration.Zero)
      else (if (current < maximum) current + increment else current, current)
    }

    backoff(linearBackoffAlgorithm(maximum)(increment))(backoffRef)(action)
  }

  def exponentialBackoff[F[_]: Timer: Monad, A](maximum: FiniteDuration)(exponent: Double, multiplier: FiniteDuration)(backoffRef: Ref[F, Int])(action: F[A]): F[A] = {
    def exponentialBackoffAlgorithm(maximum: FiniteDuration)(exponent: Double, multiplier: FiniteDuration) : BackoffAlgorithm[Int] = current => {
      if (current < 0) (0, Duration.Zero)
      else math.pow(current.toDouble, exponent) * multiplier match {
        case f: FiniteDuration if f < maximum => (current + 1, f)
        case _ => (current, maximum)
      }
    }

    backoff(exponentialBackoffAlgorithm(maximum)(exponent, multiplier))(backoffRef)(action)
  }

  def backoff[F[_]: Timer: Monad, A, B](algorithm: BackoffAlgorithm[B])(backoffRef: Ref[F, B])(action: F[A]): F[A] = for {
    result <- action
    delay <- backoffRef.modify(algorithm)
    _ <- Timer[F].sleep(delay)
  } yield result

  def resetLinearBackoff[F[_] : Monad](backoffRef: Ref[F, FiniteDuration]): F[Unit] = backoffRef.set(Duration.Zero)
  def resetExponentialBackoff[F[_] : Monad](backoffRef: Ref[F, Int]): F[Unit] = backoffRef.set(0)
}
