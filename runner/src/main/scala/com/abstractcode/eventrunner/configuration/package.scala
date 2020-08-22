package com.abstractcode.eventrunner

import cats.data.ValidatedNec
import cats.implicits._
import com.abstractcode.eventrunner.configuration.ParseError._
import org.http4s.Uri

package object configuration {
  def getOptionalUri(env: Map[String, String])(key: String): ValidatedNec[ParseError, Option[Uri]] =
    env.get(key)
      .traverse(e => Uri.fromString(e).leftMap(_ => ParseError(key, InvalidFormat)).toValidatedNec)

  def getUri(env: Map[String, String])(key: String): ValidatedNec[ParseError, Uri] = env.get(key)
    .filter(!_.isBlank)
    .map(e => Uri.fromString(e).leftMap(_ => ParseError(key, InvalidFormat)).toValidatedNec)
    .getOrElse(ParseError(key, NotProvidedOrEmpty).invalidNec)
}
