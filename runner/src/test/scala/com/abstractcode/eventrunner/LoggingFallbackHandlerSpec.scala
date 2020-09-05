package com.abstractcode.eventrunner

import cats.Show
import cats.data.Kleisli
import cats.effect.IO
import cats.effect.concurrent.Ref
import com.abstractcode.eventrunner.ProcessingError.UnknownHandler
import com.abstractcode.eventrunner.TestMessage._
import com.abstractcode.eventrunner.logging.Logged
import io.circe.Encoder
import org.specs2.matcher.MatchResult
import org.specs2.scalacheck.ScalaCheckFunction2
import org.specs2.specification.core.SpecStructure
import org.specs2.{ScalaCheck, Specification}

class LoggingFallbackHandlerSpec extends Specification with ScalaCheck {
  def is: SpecStructure =
    s2"""
         loggingFallbackHandler should
            log that a message did not have a handler $logMessageDidNotHaveHandler
            should return error $returnError
      """

  def logMessageDidNotHaveHandler: ScalaCheckFunction2[FirstTestMessage, TestMetadata, MatchResult[Any]] =
    prop {
      (testMessage: FirstTestMessage, testMetadata: TestMetadata) => {
        def buildLogged(ref: Ref[IO, List[String]]): Logged[IO, TestMetadata] = new Logged[IO, TestMetadata] {
          def log[D](logData: => D)(implicit encoder: Encoder[D]): MessageContext[IO, TestMetadata] =
            Kleisli(
              metadata => {
                val updates = List(
                  metadata.transactionId,
                  logData match {
                    case s: String if s.contains(testMetadata.messageType) => "received expected log value"
                    case _ => "did not receive expected log value"
                  }
                )
                ref.update(ls => ls ::: updates)
              }
            )

          def error(message: => String, error: => Throwable)(implicit throwableEncoder: Encoder[Throwable]): MessageContext[IO, TestMetadata] =
            Kleisli(_ => IO.raiseError(new Exception("logged as error")))
        }

        val result = for {
          ref <- Ref.of[IO, List[String]](Nil)
          logged = buildLogged(ref)
          handler = loggingFallbackHandler[IO, TestMessage, String, TestMetadata](ThrowableMonadError[IO], logged, Show[String])
          _ <- handler(testMessage).run(testMetadata).redeem(_ => (), _ => ())
          value <- ref.get
        } yield value

        result.attempt.unsafeRunSync() shouldEqual Right(List(testMetadata.transactionId, "received expected log value"))
      }
    }

  def returnError: ScalaCheckFunction2[FirstTestMessage, TestMetadata, MatchResult[Either[Throwable, Unit]]] = prop {
    (testMessage: FirstTestMessage, testMetadata: TestMetadata) => {
      implicit val logged: Logged[IO, TestMetadata] = new Logged[IO, TestMetadata] {
        def log[D](logData: => D)(implicit encoder: Encoder[D]): MessageContext[IO, TestMetadata] = Kleisli(_ => IO.unit)
        def error(message: => String, error: => Throwable)(implicit throwableEncoder: Encoder[Throwable]): MessageContext[IO, TestMetadata] = Kleisli(_ => IO.unit)
      }

      val handler: TestMessage => MessageContext[IO, TestMetadata] = loggingFallbackHandler[IO, TestMessage, String, TestMetadata](ThrowableMonadError[IO], logged, Show[String])

      val result = handler(testMessage).run(testMetadata).attempt.unsafeRunSync()

      result should beLeft(UnknownHandler(testMetadata.messageType))
    }
  }
}
