package com.abstractcode.eventrunner

import cats.data.Validated.{Invalid, Valid}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import com.abstractcode.eventrunner.MessageProcessor.MessageHandler
import com.abstractcode.eventrunner.messaging.{ParseErrors, SqsMessageSource, SqsMessageSourceConfiguration}
import software.amazon.awssdk.services._

import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}

object Main extends IOApp{
  case class TestMessage() extends Message

  val messageParser: sqs.model.Message => IO[Message] = _ => IO.pure(TestMessage())

  def loadConfiguration: IO[SqsMessageSourceConfiguration] = for {
    env <- IO.delay(sys.env)
    configuration <- SqsMessageSourceConfiguration(env) match {
      case Valid(config) => IO.pure(config)
      case Invalid(errors) => IO.raiseError(ParseErrors(errors))
    }

  } yield configuration

  def run(args: List[String]): IO[ExitCode] = {
    val handler:  MessageHandler[IO] = {
      case _: TestMessage => IO(println("Handled"))
    }

    for {
      errorBackoff <- Ref.of[IO, FiniteDuration](Duration.Zero)
      backoff = Backoff.backoff[IO, Unit](FiniteDuration(1, SECONDS))(FiniteDuration(5, SECONDS))(errorBackoff) _
      configuration <- loadConfiguration
      s <- SqsMessageSource.retrieveMessage(configuration)(messageParser)
      processor = new MessageProcessor[IO](s, handler)
      _ <- Repeater.repeat(processor.process().flatMap(_ => Backoff.resetBackoff[IO, Unit](errorBackoff))
        .handleErrorWith(t => backoff(IO(println(t)))))
    } yield ExitCode.Success
  }
}
