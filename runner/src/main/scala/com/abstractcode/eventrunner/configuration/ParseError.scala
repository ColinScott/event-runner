package com.abstractcode.eventrunner.configuration

import cats.Show
import cats.data.NonEmptyChain
import com.abstractcode.eventrunner.configuration.ParseError.Reason

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
}