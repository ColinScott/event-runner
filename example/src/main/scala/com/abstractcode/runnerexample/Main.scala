package com.abstractcode.runnerexample

import java.util.concurrent.Executors

import cats.Applicative
import cats.data.Kleisli
import cats.data.Validated.{Invalid, Valid}
import cats.effect.{Blocker, Clock, Concurrent, ContextShift, ExitCode, IO, IOApp, Sync, Timer}
import cats.syntax.all._
import com.abstractcode.eventrunner._
import com.abstractcode.eventrunner.configuration.ParseErrors
import com.abstractcode.eventrunner.logging._
import com.abstractcode.eventrunner.messaging.{retrieveFromSqs, runQueue}
import com.abstractcode.eventrunner.sqscirce.MessageParser
import com.abstractcode.runnerexample.ExampleMessages._
import com.abstractcode.runnerexample.Handlers._
import fs2.concurrent.Queue
import io.circe.Json
import software.amazon.awssdk.services.sqs.SqsClient

import scala.concurrent.ExecutionContext

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

  def startTasks[F[_]: Concurrent : ContextShift : Clock : Timer](blocker: Blocker, logBlocker: Blocker)(logQueue: Queue[F, Json]): F[ExitCode] = {
    implicit val loggedGlobal: Logged[F] = new CirceLoggedGlobal[F](logQueue)

    def runFirstQueue(configuration: ExampleConfiguration): F[Unit] = {
      val retrieve = retrieveFromSqs(blocker, configuration.sqsMessageSourceConfiguration, configuration.queueUri,  MessageParser.build[F, ExampleContainer])
      val handlers: Logged[Kleisli[F, ExampleMetadata, *]] => PartialFunction[ExampleMessage, Kleisli[F, ExampleMetadata, Unit]] =
        logged => {
          implicit val l: Logged[Kleisli[F, ExampleMetadata, *]] = logged
          firstMessageHandler[ExampleF[F, *]].orElse(secondMessageHandler[ExampleF[F, *]])
        }
      val messageProcessor = buildMessageProcessor[F, ExampleMetadata, TransactionId, MessageType, ExampleMessage, SqsClient](logQueue, retrieve, handlers)
      runQueue[F, ExampleMetadata, TransactionId, MessageType, ExampleMessage](blocker, configuration.sqsMessageSourceConfiguration, messageProcessor)
    }

    val program = for {
      _ <- loggedGlobal.log("Started")
      configuration <- loadConfiguration
      _ <- Concurrent[F].race(runFirstQueue(configuration), CirceLoggedBackend.writeLogs[F](logQueue, logBlocker))
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
