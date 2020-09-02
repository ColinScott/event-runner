package com.abstractcode

package object eventrunner {
  type MessageHandler[F[_], Container <: MessageContainer[_, _]] = Container => F[Unit]
}
