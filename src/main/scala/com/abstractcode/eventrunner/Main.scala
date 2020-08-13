package com.abstractcode.eventrunner

import cats.effect.{ExitCode, IO, IOApp}
import com.abstractcode.eventrunner.MessageProcessor.MessageHandler
import com.abstractcode.eventrunner.messaging.SqsMessageSource
import com.abstractcode.eventrunner.messaging.SqsMessageSource._
import eu.timepit.refined.refineV
import org.http4s.implicits._
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services._

object Main extends IOApp{
  case class TestMessage() extends Message

  val messageParser: sqs.model.Message => IO[Message] = _ => IO.pure(TestMessage())

  val configuration: SqsMessageSourceConfiguration = SqsMessageSourceConfiguration(
    uri"http://localhost:4576/000000000000/commands",
    Region.AP_SOUTHEAST_2,
    SqsLocalstack(uri"http://localhost:4566", AwsBasicCredentials.create("test", "test")),
    refineV[WaitTimeConstraint](20).getOrElse(throw new Exception("This is a test"))
  )

  def run(args: List[String]): IO[ExitCode] = {

    val source: IO[() => IO[Option[MessageContainer[IO]]]] = SqsMessageSource.retrieveMessage(configuration)(messageParser)

    val handler:  MessageHandler[IO] = {
      case _: TestMessage => IO(println("Handled"))
    }

    for {
      s <- source
      processor = new MessageProcessor[IO](s, handler)
      _ <- processor.process()
    } yield ExitCode.Success
  }
}
