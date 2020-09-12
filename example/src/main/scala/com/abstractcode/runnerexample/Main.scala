package com.abstractcode.runnerexample

import java.util.concurrent.Executors

import cats.data.Kleisli
import cats.data.Validated.{Invalid, Valid}
import cats.effect.concurrent.Ref
import cats.effect.{Blocker, Clock, Concurrent, ContextShift, ExitCode, IO, IOApp, Sync, Timer}
import cats.syntax.all._
import cats.Applicative
import com.abstractcode.eventrunner._
import com.abstractcode.eventrunner.configuration.ParseErrors
import com.abstractcode.eventrunner.logging._
import com.abstractcode.eventrunner.messaging.SqsMessageSource
import com.abstractcode.eventrunner.sqscirce.MessageParser
import com.abstractcode.runnerexample.ExampleMessages._
import com.abstractcode.runnerexample.Handlers._
import fs2.concurrent.Queue
import io.circe.Json
import software.amazon.awssdk.services.sqs

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{FiniteDuration, SECONDS}

object Main extends IOApp {
  type ExampleIO[A] = Kleisli[IO, ExampleMetadata, A]
  type ExampleF[F[_], A] = Kleisli[F, ExampleMetadata, A]

  def loadConfiguration[F[_]: Sync]: F[ExampleConfiguration] = for {
    env <- Sync[F].delay(sys.env)
    configuration <- ExampleConfiguration.parse(env) match {
      case Valid(config) => Applicative[F].pure(config)
      case Invalid(errors) => ThrowableMonadError[F].raiseError(ParseErrors(errors))
    }

  } yield configuration

  def compose[F[_]: ThrowableMonadError : Logged](implicit logged: Logged[ExampleF[F, *]]): MessageHandler[ExampleF[F, *], ExampleMessage] = {
    val fallback: ExampleMessage => Kleisli[F, ExampleMetadata, Unit] = loggingFallbackHandler[F, ExampleMessage, MessageType, ExampleMetadata]
    buildMessageHandler[ExampleF[F, *], ExampleMessage](firstMessageHandler[ExampleF[F, *]].orElse(secondMessageHandler[ExampleF[F, *]]), fallback)
  }

  def runQueue[F[_]: Sync : ContextShift: Clock : Timer : Logged](blocker: Blocker, configuration: ExampleConfiguration, logQueue: Queue[F, Json]): F[Unit] = {
    val messageParser: sqs.model.Message => F[ExampleContainer] = MessageParser.build[F, ExampleContainer]
    implicit val logged: CirceLogged[F, TransactionId, ExampleMetadata] = new CirceLogged[F, TransactionId, ExampleMetadata](logQueue)

    for {
      errorBackoff <- Ref.of[F, Int](0)
      backoff = Backoff.exponentialBackoff[F, Unit](FiniteDuration(30, SECONDS))(1.5, FiniteDuration(1, SECONDS))(errorBackoff) _
      _ <- SqsMessageSource.clientResource[F](blocker)(configuration.sqsMessageSourceConfiguration).use {
        sqsClient => {
          val retrieve = SqsMessageSource.retrieveMessage[F, ExampleContainer](blocker, configuration.sqsMessageSourceConfiguration)(configuration.queueUri, messageParser)(sqsClient)
          val processor = new MessageProcessor[F, ExampleMessage, ExampleMetadata](retrieve, compose[F])
          for {
            _ <- Repeater.repeat(processor.process().flatMap(_ => Backoff.resetExponentialBackoff[F](errorBackoff))
              .handleErrorWith(t => backoff(Logged[F].log(t))))
          } yield ()
        }
      }
    } yield ()
  }

  def startTasks[F[_]: Concurrent : ContextShift : Clock : Timer](blocker: Blocker, logBlocker: Blocker)(logQueue: Queue[F, Json]): F[ExitCode] = {
    implicit val loggedGlobal: Logged[F] = new CirceLoggedGlobal[F](logQueue)
    val program = for {
      _ <- loggedGlobal.log("Started")
      configuration <- loadConfiguration
      _ <- Concurrent[F].race(runQueue(blocker, configuration, logQueue), CirceLoggedBackend.writeLogs[F](logQueue, logBlocker))
    } yield ExitCode.Success

    ThrowableMonadError[F].recoverWith(program) {
      case ParseErrors(errors) => for {
        _ <- loggedGlobal.log(errors)
      } yield ExitCode.Error
    }
  }

  def run(args: List[String]): IO[ExitCode] = {
    val blocker = Blocker.liftExecutionContext(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))
    val logBlocker = Blocker.liftExecutionContext(ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1)))

    for {
      logQueue <- Queue.circularBuffer[IO, Json](1024)
      result <- startTasks[IO](blocker, logBlocker)(logQueue)
    } yield result
  }
}
