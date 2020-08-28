package com.abstractcode.eventrunner.configuration

import cats.Show
import cats.data.NonEmptyChain
import com.abstractcode.eventrunner.configuration.ParseError.Reason
import io.circe.syntax._
import io.circe.{Encoder, Json}

case class ParseError(environmentVariable: String, reason: Reason)
case class ParseErrors(errors: NonEmptyChain[ParseError]) extends Exception

object ParseError {
  sealed trait Reason
  case object NotProvidedOrEmpty extends Reason
  case object InvalidFormat extends Reason

  implicit val showParseError: Show[ParseError] = Show.show {
    case ParseError(variable, NotProvidedOrEmpty) => s"Environment variable $variable was not provided or is empty"
    case ParseError(variable, InvalidFormat) => s"Format of environment variable $variable is invalid"
  }

  implicit val parseErrorEncoder: Encoder[ParseError] = (parseError: ParseError) => Json.obj(
    ("environmentVariable", parseError.environmentVariable.asJson),
    ("reason", (parseError.reason match { case NotProvidedOrEmpty => "Not provided or empty" case InvalidFormat => "Invalid Format"}).asJson)
  )
}