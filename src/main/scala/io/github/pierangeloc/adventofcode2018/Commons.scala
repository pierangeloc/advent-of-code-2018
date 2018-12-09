package io.github.pierangeloc.adventofcode2018

import java.nio.charset.StandardCharsets
import java.nio.file.Paths

import cats.effect.{ContextShift, IO, Sync}
import fs2.Stream
import fs2.io.file

import scala.concurrent.ExecutionContextExecutor

/**
  *
  * advent-of-code-2018 - 2018-12-04
  * Created with ♥ in Amsterdam
  */
object Commons {
  def readLines[F[_] : Sync : ContextShift]
  (path: String, blockingEC: ExecutionContextExecutor): Stream[F, String] =
    file.readAll[F](Paths.get(path), blockingEC, 1024)
      .split(byte => byte == '\n'.toByte)
      .map(chunk => new String(chunk.toArray[Byte], StandardCharsets.UTF_8))

  def readChars[F[_] : Sync : ContextShift]
  (path: String, blockingEC: ExecutionContextExecutor): Stream[F, Char] =
    file.readAll[F](Paths.get(path), blockingEC, 1024)
      .map(_.toChar)

  def putStrln(s: String) = IO.delay(println(s))

}
