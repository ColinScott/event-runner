package com.abstractcode.runnerexample

import java.util.concurrent.Executors

import cats.Applicative
import cats.data.Validated.{Invalid, Valid}
import cats.effect.concurrent.Ref
import cats.effect.{Blocker, Clock, Concurrent, ContextShift, ExitCode, IO, IOApp, Sync, Timer}
import cats.syntax.all._
import com.abstractcode.eventrunner.configuration.ParseErrors
import com.abstractcode.eventrunner.logging.{CirceLogged, Logged, _}
import com.abstractcode.eventrunner.messaging.SqsMessageSource
import com.abstractcode.eventrunner.sqscirce.MessageParser
import com.abstractcode.eventrunner.{ThrowableMonadError, _}
import com.abstractcode.runnerexample.ExampleMessages._
import com.abstractcode.runnerexample.Handlers._
import fs2.concurrent.Queue
import io.circe.Json
import software.amazon.awssdk.services.sqs

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{FiniteDuration, SECONDS}

object Main extends IOApp {
  def loadConfiguration[F[_]: Sync]: F[ExampleConfiguration] = for {
    env <- Sync[F].delay(sys.env)
    configuration <- ExampleConfiguration.parse(env) match {
      case Valid(config) => Applicative[F].pure(config)
      case Invalid(errors) => ThrowableMonadError[F].raiseError(ParseErrors(errors))
    }

  } yield configuration

  def buildHandler[F[_]: ThrowableMonadError](implicit logged: Logged[F, ExampleMetadata]): ContainerHandler[F, ExampleContainer] =
    buildMessageHandler(firstMessageHandler orElse secondMessageHandler, loggingFallbackHandler[F, ExampleMessage, MessageType, ExampleMetadata])

  def runQueue[F[_]: Sync : ContextShift : Timer](blocker: Blocker, configuration: ExampleConfiguration)(implicit logged: Logged[F, ExampleMetadata], loggedGlobal: LoggedGlobal[F]): F[Unit] = {
    val messageParser: sqs.model.Message => F[ExampleContainer] = MessageParser.build[F, ExampleContainer]

    for {
      errorBackoff <- Ref.of[F, Int](0)
      backoff = Backoff.exponentialBackoff[F, Unit](FiniteDuration(30, SECONDS))(1.5, FiniteDuration(1, SECONDS))(errorBackoff) _
      _ <- SqsMessageSource.clientResource[F](blocker)(configuration.sqsMessageSourceConfiguration).use {
        sqsClient => {
          val retrieve = SqsMessageSource.retrieveMessage[F, ExampleContainer](blocker, configuration.sqsMessageSourceConfiguration)(configuration.queueUri, messageParser)(sqsClient)
          val processor = new MessageProcessor[F, ExampleContainer](retrieve, buildHandler)
          for {
            _ <- Repeater.repeat(processor.process().flatMap(_ => Backoff.resetExponentialBackoff[F](errorBackoff))
              .handleErrorWith(t => backoff(loggedGlobal.log(t))))
          } yield ()
        }
      }
    } yield ()
  }

  def startTasks[F[_]: Concurrent : ContextShift : Clock : Timer](blocker: Blocker, logBlocker: Blocker)(queue: Queue[F, Json]): F[ExitCode] = {
    implicit val logged: Logged[F, ExampleMetadata] = new CirceLogged[F, TransactionId, ExampleMetadata](queue)
    implicit val loggedGlobal: LoggedGlobal[F] = new CirceLoggedGlobal[F](queue)
    val program = for {
      _ <- loggedGlobal.log("Started")
      configuration <- loadConfiguration
      _ <- Concurrent[F].race(runQueue[F](blocker, configuration), CirceLoggedBackend.writeLogs[F](queue, logBlocker))
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
