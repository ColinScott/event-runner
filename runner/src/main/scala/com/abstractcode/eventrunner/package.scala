package com.abstractcode

package object eventrunner {
  type MessageHandler[F[_], Container <: MessageContainer[_, _]] = PartialFunction[Container, F[Unit]]
}
