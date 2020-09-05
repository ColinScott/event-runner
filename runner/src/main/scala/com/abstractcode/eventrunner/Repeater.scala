package com.abstractcode.eventrunner

import cats.Monad
import cats.implicits._

object Repeater {
  def repeat[F[_]: ThrowableMonadError](process: F[Unit]): F[Unit] = {
    val wrapper: Unit => F[Either[Unit, Unit]] = _ => process.attempt.map(_ => ().asLeft)

    Monad[F].tailRecM(())(wrapper)
  }
}
