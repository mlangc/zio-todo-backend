package com.schuwalow.todo.log

import com.github.mlangc.slf4zio.api.Logging
import zio.ZLayer

object Slf4jLogging {
  val layer: ZLayer[Any, Nothing, Logging] = Logging.forClass(getClass)
}
