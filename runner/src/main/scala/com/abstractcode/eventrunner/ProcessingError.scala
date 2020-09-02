package com.abstractcode.eventrunner

sealed trait ProcessingError extends Exception

object ProcessingError {
  case class UnknownHandler[T](messageType: T) extends ProcessingError
}
