package com.abstractcode.eventrunner.logging

import java.time.{Instant, ZoneOffset, ZonedDateTime}

import cats.Monad
import cats.data.{Chain, Kleisli}
import cats.effect.concurrent.{MVar2, Ref}
import cats.effect.{Clock, Concurrent, Sync}
import cats.implicits._
import io.circe.syntax._
import io.circe.{Encoder, Json}

import scala.concurrent.duration.MILLISECONDS

trait Logged[F[_], T] {
  def log[D](logData: => D)(implicit encoder: Encoder[D]): Kleisli[F, T, Unit]
  def logGlobal[D](logData: => D)(implicit encoder: Encoder[D]): F[Unit]
}

object Logged {
  case class LogData()

  def apply[F[_], T](implicit logged: Logged[F, T]): Logged[F, T] = logged
}

class CirceLogged[F[_]: Monad : Clock : Concurrent, T](signal: MVar2[F, Unit], logItems: Ref[F, Chain[Json]])(implicit transactionIdEncoder: Encoder[T]) extends Logged[F, T] {
  case class LogFormat[D](transactionId: Option[T], timestamp: ZonedDateTime, logData: D)

  def log[D](logData: => D)(implicit encoder: Encoder[D]): Kleisli[F, T, Unit] =
    Kleisli(transactionId => logImpl[D](Some(transactionId), logData))

  def logGlobal[D](logData: => D)(implicit encoder: Encoder[D]): F[Unit] = logImpl[D](None, logData)

  def logImpl[D](transactionId: Option[T], logData: D)(implicit encoder: Encoder[D]): F[Unit] = {
    implicit def logFormatEncoder: Encoder[LogFormat[D]] =
      (logFormat: LogFormat[D]) => Json.obj(
        ("timestamp", logFormat.timestamp.asJson),
        ("data", logFormat.logData.asJson),
        ("transactionId", logFormat.transactionId.asJson)
      ).dropNullValues

    for {
      now <- Clock[F].realTime(MILLISECONDS)
      timestamp = ZonedDateTime.ofInstant(Instant.ofEpochMilli(now), ZoneOffset.UTC)
      logFormat = LogFormat(transactionId, timestamp, logData).asJson
      _ <- logItems.update(items => items.append(logFormat))
      _ <- signal.tryPut(())
    } yield ()
  }
}

object CirceLogged {
  def writeLogs[F[_]: Concurrent](signal: MVar2[F, Unit], logItems: Ref[F, Chain[Json]]): F[Unit] = for {
    _ <- signal.take
    _ <- flushLogs[F](logItems)
  } yield ()

  def flushLogs[F[_]: Concurrent](logItems: Ref[F, Chain[Json]]): F[Unit] = for {
    items <- logItems.getAndSet(Chain.empty)
    _ <- Sync[F].delay(println(items.map(_.noSpaces).mkString_("\n")))
  } yield ()
}