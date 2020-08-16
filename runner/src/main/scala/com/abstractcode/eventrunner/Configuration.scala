package com.abstractcode.eventrunner

import cats.Show
import cats.data.{NonEmptyChain, ValidatedNec}
import cats.implicits._
import com.abstractcode.eventrunner.ParseError.{InvalidFormat, NotProvidedOrEmpty, Reason}
import org.http4s.Uri

object Configuration {
  def getOptionalUri(env: Map[String, String])(key: String): ValidatedNec[ParseError, Option[Uri]] =
    env.get(key)
      .traverse(e => Uri.fromString(e).leftMap(_ => ParseError(key, InvalidFormat)).toValidatedNec)

  def getUri(env: Map[String, String])(key: String): ValidatedNec[ParseError, Uri] = env.get(key)
    .filter(!_.isBlank)
    .map(e => Uri.fromString(e).leftMap(_ => ParseError(key, InvalidFormat)).toValidatedNec)
    .getOrElse(ParseError(key, NotProvidedOrEmpty).invalidNec)
}

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