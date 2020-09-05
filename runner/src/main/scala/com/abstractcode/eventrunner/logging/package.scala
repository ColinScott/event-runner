package com.abstractcode.eventrunner

import io.circe.syntax._
import io.circe.{Encoder, Json}

package object logging {
  implicit val throwableEncoder: Encoder[Throwable] = throwableEncoderInner

  def throwableEncoderInner: Encoder[Throwable] = (exception: Throwable) => Json.obj(
    ("exceptionType", exception.getClass.getName.asJson),
    ("message", exception.getLocalizedMessage.asJson),
    ("cause", if (exception.getCause!= null) exception.getCause.asJson else Json.Null)
  ).dropNullValues
}
