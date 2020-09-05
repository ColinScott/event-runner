package com.abstractcode.eventrunner.logging

import java.time.{Instant, ZoneOffset, ZonedDateTime}

import cats.Monad
import cats.data.{Chain, Kleisli}
import cats.effect.concurrent.{MVar2, Ref}
import cats.effect.{Clock, Concurrent, Sync}
import cats.syntax.all._
import com.abstractcode.eventrunner.{MessageContext, Metadata}
import io.circe.syntax._
import io.circe.{Encoder, Json}

import scala.concurrent.duration.MILLISECONDS

trait Logged[F[_], -MD <: Metadata[_, _]] {
  def log[D](logData: => D)(implicit encoder: Encoder[D]): MessageContext[F, MD]
  def error(message: => String, error: => Throwable)(implicit throwableEncoder: Encoder[Throwable]): MessageContext[F, MD]
}

object Logged {
  def apply[F[_], MD <: Metadata[_, _]](implicit logged: Logged[F, MD]): Logged[F, MD] = logged
}

trait LoggedGlobal[F[_]] {
  def log[D](logData: => D)(implicit encoder: Encoder[D]): F[Unit]
  def error(message: => String, error: => Throwable)(implicit throwableEncoder: Encoder[Throwable]): F[Unit]
}

object LoggedGlobal {
  def apply[F[_]](implicit loggedGlobal: LoggedGlobal[F]): LoggedGlobal[F] = loggedGlobal
}

class CirceLogged[F[_]: Monad : Clock, T, MD <: Metadata[T, _]](logSignal: MVar2[F, Unit], logItemsRef: Ref[F, Chain[Json]])(implicit idEncoder: Encoder[T]) extends Logged[F, MD] with CirceLoggedBackend[F] {
  val signal: MVar2[F, Unit] = logSignal
  val logItems: Ref[F, Chain[Json]] = logItemsRef

  def log[D](logData: => D)(implicit encoder: Encoder[D]): MessageContext[F, MD] =
    Kleisli(metadata => logImpl[D](Some(metadata.transactionId.asJson), logData))

  def error(message: => String, error: => Throwable)(implicit throwableEncoder: Encoder[Throwable]): MessageContext[F, MD] =
    log(ErrorWrapper(message, error))
}

class CirceLoggedGlobal[F[_]: Monad : Clock](logSignal: MVar2[F, Unit], logItemsRef: Ref[F, Chain[Json]]) extends LoggedGlobal[F] with CirceLoggedBackend[F] {
  val signal: MVar2[F, Unit] = logSignal
  val logItems: Ref[F, Chain[Json]] = logItemsRef

  def log[D](logData: => D)(implicit encoder: Encoder[D]): F[Unit] =
    logImpl[D](None, logData)

  def error(message: => String, error: => Throwable)(implicit throwableEncoder: Encoder[Throwable]): F[Unit] =
    log(ErrorWrapper(message, error))
}

trait CirceLoggedBackend[F[_]] {
  val signal: MVar2[F, Unit]
  val logItems: Ref[F, Chain[Json]]

  case class LogFormat[D](transactionId: Option[Json], timestamp: ZonedDateTime, logData: D)
  case class ErrorWrapper(message: String, error: Throwable)

  implicit def errorWrapperEncoder(implicit throwableEncoder: Encoder[Throwable]): Encoder[ErrorWrapper] = (e: ErrorWrapper) => Json.obj(
    ("message", e.message.asJson),
    ("error", e.error.asJson)
  )

  def logImpl[D](transactionId: Option[Json], logData: D)(implicit encoder: Encoder[D], M: Monad[F], C: Clock[F]): F[Unit] = {
    implicit def logFormatEncoder: Encoder[LogFormat[D]] =
      (logFormat: LogFormat[D]) => Json.obj(
        ("timestamp", logFormat.timestamp.asJson),
        ("data", logFormat.logData.asJson),
        ("transactionId", logFormat.transactionId.getOrElse(Json.Null))
      ).dropNullValues

    for {
      now <- C.realTime(MILLISECONDS)
      timestamp = ZonedDateTime.ofInstant(Instant.ofEpochMilli(now), ZoneOffset.UTC)
      logFormat = LogFormat(transactionId, timestamp, logData).asJson
      _ <- logItems.update(items => items.append(logFormat))
      _ <- signal.tryPut(())
    } yield ()
  }
}

object CirceLoggedBackend {
  def writeLogs[F[_]: Concurrent](signal: MVar2[F, Unit], logItems: Ref[F, Chain[Json]]): F[Unit] = for {
    _ <- signal.take
    _ <- flushLogs[F](logItems)
  } yield ()

  def flushLogs[F[_]: Concurrent](logItems: Ref[F, Chain[Json]]): F[Unit] = for {
    items <- logItems.getAndSet(Chain.empty)
    _ <- Sync[F].delay(println(items.map(_.noSpaces).mkString_("\n")))
  } yield ()
}