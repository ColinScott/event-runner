package com.abstractcode.runnerexample

import java.util.UUID
import java.util.concurrent.Executors

import cats.{Applicative, MonadError}
import cats.data.Chain
import cats.data.Validated.{Invalid, Valid}
import cats.effect.concurrent.{MVar, MVar2, Ref}
import cats.effect.{Blocker, Clock, Concurrent, ContextShift, ExitCode, IO, IOApp, Sync, Timer}
import cats.implicits._
import com.abstractcode.eventrunner._
import com.abstractcode.eventrunner.circe.MessageContainerDecoder
import com.abstractcode.eventrunner.configuration.ParseErrors
import com.abstractcode.eventrunner.logging.{CirceLogged, Logged, _}
import com.abstractcode.eventrunner.messaging.SqsMessageSource
import com.abstractcode.eventrunner.sqscirce.MessageParser
import io.circe.{Decoder, HCursor, Json}
import software.amazon.awssdk.services.sqs

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.{FiniteDuration, MILLISECONDS, SECONDS}

object Main extends IOApp {

  sealed trait ExampleMessage
  case class TestMessage(count: Int) extends ExampleMessage

  case class ExampleMetadata(transactionId: UUID, messageType: String) extends Metadata[UUID, String]

  type ExampleContainer = MessageContainer[ExampleMessage, ExampleMetadata]

  val metadataDecoder: Decoder[ExampleMetadata] = (c: HCursor) => {
    val metadata = c.downField("metadata")

    for {
      transactionId <- metadata.downField("transactionId").as[UUID]
      messageType <- metadata.downField("messageType").as[String]
    } yield ExampleMetadata(transactionId, messageType)
  }

  val testMessageDecoder: Decoder[ExampleMessage] = (c: HCursor) => for {
    count <- c.downField("count").as[Int]
  } yield TestMessage(count)

  val messageDecoders: String => Option[Decoder[ExampleMessage]] = {
    case "test" => Some(testMessageDecoder)
    case _ => None
  }

  implicit val containerDecoder: Decoder[ExampleContainer] =
    MessageContainerDecoder.build(messageDecoders)(metadataDecoder)

  def loadConfiguration[F[_]: Sync]: F[ExampleConfiguration] = for {
    env <- Sync[F].delay(sys.env)
    configuration <- ExampleConfiguration.parse(env) match {
      case Valid(config) => Applicative[F].pure(config)
      case Invalid(errors) => MonadError[F, Throwable].raiseError(ParseErrors(errors))
    }

  } yield configuration

  def runQueue[F[_]: Sync : ContextShift : Timer](blocker: Blocker, configuration: ExampleConfiguration)(implicit logged: Logged[F, UUID]): F[Unit] = {
    val messageParser: sqs.model.Message => F[ExampleContainer] = MessageParser.build[F, ExampleContainer]

    for {
      errorBackoff <- Ref.of[F, Int](0)
      backoff = Backoff.exponentialBackoff[F, Unit](FiniteDuration(1, SECONDS))(1.5, FiniteDuration(1, MILLISECONDS))(errorBackoff) _
      _ <- SqsMessageSource.clientResource[F](blocker)(configuration.sqsMessageSourceConfiguration).use {
        sqsClient => {
          val retrieve = SqsMessageSource.retrieveMessage[F, ExampleContainer](blocker, configuration.sqsMessageSourceConfiguration)(configuration.queueUri, messageParser)(sqsClient)
          val processor = new MessageProcessor[F, ExampleContainer](retrieve, {
            case container => logged.log(s"Handled ${container.metadata} ${container.message}").run(container.metadata.transactionId)
          })
          for {
            _ <- Repeater.repeat(processor.process().flatMap(_ => Backoff.resetExponentialBackoff[F](errorBackoff))
              .handleErrorWith(t => backoff(logged.logGlobal(t))))
          } yield ()
        }
      }
    } yield ()
  }

  def runLogPrinter[F[_]: Concurrent](logSignal: MVar2[F, Unit], logItems: Ref[F, Chain[Json]]): F[Unit] = for {
    _ <- Repeater.repeat(CirceLogged.writeLogs[F](logSignal, logItems))
  } yield ()

  def startTasks[F[_]: Concurrent : ContextShift : Clock : Timer](blocker: Blocker, logBlocker: Blocker)(logSignal: MVar2[F, Unit], logItems: Ref[F, Chain[Json]]): F[ExitCode] = {
    implicit val logged: Logged[F, UUID] = new CirceLogged[F, UUID](logSignal, logItems)
    val program = for {
      configuration <- loadConfiguration
      _ <- Concurrent[F].race(runQueue[F](blocker, configuration), logBlocker.blockOn(runLogPrinter[F](logSignal, logItems)))
    } yield ExitCode.Success

    val recovered = MonadError[F, Throwable].recoverWith(program) {
      case ParseErrors(errors) => for {
        _ <- logged.logGlobal(errors)
      } yield ExitCode.Error
    }

    Sync[F].guarantee(recovered)(CirceLogged.flushLogs[F](logItems))
  }

  def run(args: List[String]): IO[ExitCode] = {
    val blocker = Blocker.liftExecutionContext(ExecutionContext.fromExecutor(Executors.newCachedThreadPool()))
    val logBlocker = Blocker.liftExecutionContext(ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1)))

    for {
      logSignal <- MVar[IO].empty[Unit]
      logItems <- Ref.of[IO, Chain[Json]](Chain.empty)

      result <- startTasks[IO](blocker, logBlocker)(logSignal, logItems)
    } yield result
  }
}
