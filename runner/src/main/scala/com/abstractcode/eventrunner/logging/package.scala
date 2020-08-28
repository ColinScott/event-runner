package com.abstractcode.eventrunner

import io.circe.syntax._
import io.circe.{Encoder, Json}

package object logging {
  implicit val throwableEncoder: Encoder[Throwable] = (exception: Throwable) => Json.obj(
    ("exceptionType", exception.getClass.getName.asJson),
    (
      "message", {
      val message = exception.getLocalizedMessage
      Json.fromString(if (message != null) message else "")
    })
  )
}
