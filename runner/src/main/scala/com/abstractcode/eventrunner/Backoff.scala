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

  def backoff[F[_]: Timer: Monad, A, B](algorithm: BackoffAlgorithm[B])(backoffRef: Ref[F, B])(action: F[A]): F[A] = for {
    result <- action
    delay <- backoffRef.modify(algorithm)
    _ <- Timer[F].sleep(delay)
  } yield result

  def resetBackoff[F[_] : Monad, A](backoffRef: Ref[F, FiniteDuration]): F[Unit] = backoffRef.set(Duration.Zero)
}
