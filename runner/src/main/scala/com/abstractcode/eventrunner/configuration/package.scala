package com.abstractcode.eventrunner

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits._
import com.abstractcode.eventrunner.configuration.ParseError._
import org.http4s.Uri

package object configuration {
  def getUri(env: Map[String, String])(key: String): Validated[ParseError, Uri] = env.get(key)
    .filter(!_.isBlank)
    .map(e => Uri.fromString(e).leftMap(_ => InvalidFormat(key)).toValidated)
    .getOrElse(NotProvidedOrEmpty(key).invalid)
  def getUriNec(env: Map[String, String])(key: String): ValidatedNec[ParseError, Uri] = getUri(env)(key).leftMap[NonEmptyChain[ParseError]](NonEmptyChain(_))

  def getString(env: Map[String, String])(key: String): Validated[ParseError, String] = env.get(key)
    .filter(!_.isBlank)
    .map(_.valid)
    .getOrElse(NotProvidedOrEmpty(key).invalid)
}
