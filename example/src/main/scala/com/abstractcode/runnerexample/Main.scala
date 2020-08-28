package com.abstractcode.runnerexample

import cats.data.Validated.{Invalid, Valid}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.abstractcode.eventrunner._
import com.abstractcode.eventrunner.circe.MessageContainerDecoder
import com.abstractcode.eventrunner.configuration.ParseErrors
import com.abstractcode.eventrunner.logging.{ConsoleLogged, Logged, _}
import com.abstractcode.eventrunner.messaging.SqsMessageSource
import com.abstractcode.eventrunner.sqscirce.MessageParser
import io.circe.{Decoder, HCursor}
import software.amazon.awssdk.services.sqs

import scala.concurrent.duration.{FiniteDuration, SECONDS}

object Main extends IOApp {
  sealed trait ExampleMessage
  case class TestMessage(count: Int) extends ExampleMessage

  case class ExampleMetadata(transactionId: String, messageType: String) extends MetadataWithType[String]

  type ExampleContainer = MessageContainer[ExampleMessage, ExampleMetadata]

  val metadataDecoder: Decoder[ExampleMetadata] = (c: HCursor) => {
    val metadata = c.downField("metadata")

    for {
      transactionId <- metadata.downField("transactionId").as[String]
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

  val messageParser: sqs.model.Message => IO[ExampleContainer] = MessageParser.build[IO, ExampleContainer]

  def loadConfiguration: IO[ExampleConfiguration] = for {
    env <- IO.delay(sys.env)
    configuration <- ExampleConfiguration.parse(env) match {
      case Valid(config) => IO.pure(config)
      case Invalid(errors) => IO.raiseError(ParseErrors(errors))
    }

  } yield configuration

  def run(args: List[String]): IO[ExitCode] = {
    implicit val logged: Logged[IO] = new ConsoleLogged[IO]()

    val handler: MessageHandler[IO, ExampleContainer] = {
      case container => logged.log(s"Handled ${container.metadata} ${container.message}")
    }

    val result = for {
      errorBackoff <- Ref.of[IO, Int](0)
      backoff = Backoff.exponentialBackoff[IO, Unit](FiniteDuration(30, SECONDS))(1.5, FiniteDuration(1, SECONDS))(errorBackoff) _
      configuration <- loadConfiguration
      s <- SqsMessageSource.retrieveMessage(configuration.sqsMessageSourceConfiguration)(configuration.queueUri)(messageParser)
      processor = new MessageProcessor[IO, ExampleContainer](s, handler)
      _ <- Repeater.repeat(processor.process().flatMap(_ => Backoff.resetExponentialBackoff[IO](errorBackoff))
        .handleErrorWith(t => backoff(logged.log(t))))
    } yield ExitCode.Success

    result.recoverWith {
      case ParseErrors(errors) => for {
        _ <- logged.log(errors)
      } yield ExitCode.Error
    }
  }
}
