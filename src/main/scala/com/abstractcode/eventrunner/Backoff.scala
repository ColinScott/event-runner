package com.abstractcode.eventrunner

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Sync, Timer}
import cats.implicits._

import scala.concurrent.duration.{Duration, FiniteDuration}

object Backoff {
  def backoff[F[_]: Timer: Sync, A](increment: FiniteDuration)(maximum: FiniteDuration)(backoffRef: Ref[F, FiniteDuration])(action: F[A]): F[A] = for {
    a <- action
    delay <- backoffRef.modify(current => {
      if (current < Duration.Zero) (Duration.Zero, Duration.Zero)
      else (if (current < maximum) current + increment else current, current)
    })
    _ <- Sync[F].delay(println(s"Backoff: $delay"))
    _ <- Timer[F].sleep(delay)
  } yield a

  def resetBackoff[F[_] : Monad, A](backoffRef: Ref[F, FiniteDuration]): F[Unit] = backoffRef.set(Duration.Zero)
}
