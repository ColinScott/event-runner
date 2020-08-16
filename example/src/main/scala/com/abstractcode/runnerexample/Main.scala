package com.abstractcode.runnerexample

import cats.data.Validated.{Invalid, Valid}
import cats.effect.concurrent.Ref
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.abstractcode.eventrunner.MessageProcessor.MessageHandler
import com.abstractcode.eventrunner.messaging.SqsMessageSource
import com.abstractcode.eventrunner._
import software.amazon.awssdk.services.sqs

import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}

object Main extends IOApp {
  case class TestMessage() extends Message

  val messageParser: sqs.model.Message => IO[Message] = _ => IO.pure(TestMessage())

  def loadConfiguration: IO[ExampleConfiguration] = for {
    env <- IO.delay(sys.env)
    configuration <- ExampleConfiguration.parse(env) match {
      case Valid(config) => IO.pure(config)
      case Invalid(errors) => IO.raiseError(ParseErrors(errors))
    }

  } yield configuration

  def run(args: List[String]): IO[ExitCode] = {
    val handler:  MessageHandler[IO] = {
      case _: TestMessage => IO(println("Handled"))
    }

    val result = for {
      errorBackoff <- Ref.of[IO, FiniteDuration](Duration.Zero)
      backoff = Backoff.backoff[IO, Unit](FiniteDuration(1, SECONDS))(FiniteDuration(5, SECONDS))(errorBackoff) _
      configuration <- loadConfiguration
      s <- SqsMessageSource.retrieveMessage(configuration.sqsMessageSourceConfiguration)(configuration.queueUri)(messageParser)
      processor = new MessageProcessor[IO](s, handler)
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
