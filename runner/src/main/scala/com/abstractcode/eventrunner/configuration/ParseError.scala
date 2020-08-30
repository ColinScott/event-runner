package com.abstractcode.eventrunner.configuration

import cats.Show
import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import io.circe.syntax._
import io.circe.{Encoder, Json}

sealed trait ParseError
case class ParseErrors(errors: NonEmptyChain[ParseError]) extends Exception

object ParseError {
  case class NotProvidedOrEmpty(environmentVariable: String) extends ParseError
  case class InvalidFormat(environmentVariable: String) extends ParseError
  case class InvalidSection(description: String, subErrors: Chain[ParseError]) extends ParseError

  implicit val showParseError: Show[ParseError] = Show.show {
    case NotProvidedOrEmpty(variable) => s"Environment variable $variable was not provided or is empty"
    case InvalidFormat(variable) => s"Format of environment variable $variable is invalid"
    case InvalidSection(description, subErrors) => s"$description\n${subErrors.map(showError).mkString_("- ", "\n", "")}"
  }

  private def showError(p: ParseError): String = p.show

  implicit val parseErrorEncoder: Encoder[ParseError] = {
    case NotProvidedOrEmpty(variable) => Json.obj(("environmentVariable", variable.asJson), ("reason", "Not provided or empty".asJson))
    case InvalidFormat(variable) => Json.obj(("environmentVariable", variable.asJson), ("reason", " \"Invalid Format\"".asJson))
    case InvalidSection(description, subErrors) => Json.obj(
      ("section", description.asJson),
      ("subErrors", encodeParseErrors(subErrors))
    )
  }

  private def encodeParseErrors(errors: Chain[ParseError]): Json = errors.asJson
}