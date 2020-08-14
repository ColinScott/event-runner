package com.abstractcode.eventrunner

import cats.{Monad, MonadError}
import cats.implicits._

object Repeater {
  def repeat[F[_]](process: F[Unit])(implicit monadError: MonadError[F, Throwable]): F[Unit] = {
    val wrapper: Unit => F[Either[Unit, Unit]] = _ => process.attempt.map(_ => ().asLeft)

    Monad[F].tailRecM(())(wrapper)
  }
}
