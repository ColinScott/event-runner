package com.abstractcode.eventrunner.logging

import java.time.{Instant, ZoneOffset, ZonedDateTime}

import cats.effect.{Clock, Sync}
import cats.implicits._
import io.circe.{Encoder, Json}
import io.circe.syntax._

import scala.concurrent.duration.MILLISECONDS

trait Logged[F[_]] {
  def log[D](logData: => D)(implicit encoder: Encoder[D]): F[Unit]
}

object Logged {
  case class LogData()

  def apply[F[_]](implicit logged: Logged[F]): Logged[F] = logged
}

class ConsoleLogged[F[_]: Sync : Clock] extends Logged[F] {
  case class LogFormat[D](timestamp: ZonedDateTime, logData: D)

  implicit def logFormatEncoder[D](implicit logEncoder: Encoder[D]): Encoder[LogFormat[D]] =
    (logFormat: LogFormat[D]) => Json.obj(
      ("timestamp", logFormat.timestamp.asJson),
      ("data", logFormat.logData.asJson)
    )

  def log[D](logData: => D)(implicit encoder: Encoder[D]): F[Unit] = for {
    now <- Clock[F].realTime(MILLISECONDS)
    timestamp = ZonedDateTime.ofInstant(Instant.ofEpochMilli(now), ZoneOffset.UTC)
    logFormat = LogFormat(timestamp, logData).asJson.noSpaces
    _ <- Sync[F].delay(println(logFormat))
  } yield ()
}