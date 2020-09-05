package com.abstractcode

import com.abstractcode.eventrunner.logging.Logged
import com.abstractcode.runnerexample.ExampleMessages.ExampleMetadata

package object runnerexample {
  type ExampleLogged[F[_]] = Logged[F, ExampleMetadata]

  object ExampleLogged {
    def apply[F[_]](implicit exampleLogged: ExampleLogged[F]): ExampleLogged[F] = exampleLogged
  }
}
