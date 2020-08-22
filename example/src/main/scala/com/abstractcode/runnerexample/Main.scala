package com.abstractcode.runnerexample

import cats.data.Validated.{Invalid, Valid}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.abstractcode.eventrunner._
import com.abstractcode.eventrunner.circe.MessageContainerDecoder
import com.abstractcode.eventrunner.configuration.ParseErrors
import com.abstractcode.eventrunner.messaging.SqsMessageSource
import com.abstractcode.eventrunner.sqscirce.MessageParser
import io.circe.{Decoder, HCursor}
import software.amazon.awssdk.services.sqs

import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}

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
    val handler: MessageHandler[IO, ExampleContainer] = {
      case container => IO(println(s"Handled ${container.metadata} ${container.message}"))
    }

    val result = for {
      errorBackoff <- Ref.of[IO, FiniteDuration](Duration.Zero)
      backoff = Backoff.backoff[IO, Unit](FiniteDuration(1, SECONDS))(FiniteDuration(5, SECONDS))(errorBackoff) _
      configuration <- loadConfiguration
      s <- SqsMessageSource.retrieveMessage(configuration.sqsMessageSourceConfiguration)(configuration.queueUri)(messageParser)
      processor = new MessageProcessor[IO, ExampleContainer](s, handler)
      _ <- Repeater.repeat(processor.process().flatMap(_ => Backoff.resetBackoff[IO, Unit](errorBackoff))
        .handleErrorWith(t => backoff(IO(println(t)))))
    } yield ExitCode.Success

    result.recoverWith {
      case ParseErrors(errors) => for {
        _ <- IO(println(errors.map(_.show).foldLeft("")((b, a) => if (b.isEmpty) a else s"$b\n$a")))
      } yield ExitCode.Error
    }
  }
}
