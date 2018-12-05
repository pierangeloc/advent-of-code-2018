package io.github.pierangeloc.adventofcode2018

import java.util.concurrent.Executors

import cats.implicits._
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Sync}
import fs2._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

/**
  *
  * advent-of-code-2018 - 2018-12-03
  * Created with ♥ in Amsterdam
  */



object Day1 extends IOApp {

  implicit val blockingEC: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1))

  type Result[A] = Either[String, A]
  val path = "/Users/pierangelo.cecchetto/Documents/projects/scala/piero/advent-of-code-2018/input/day1-1.txt"

  def readInts[F[_] : Sync : ContextShift]: Stream[F, Either[Throwable, Int]] =
    Commons.readLines[F](path, blockingEC).map { s =>
      Either.catchNonFatal(s.toInt)
    }

  def addLines[F[_]]: Pipe[F, Either[Throwable, Int], Result[Int]] = _.fold[Result[Int]](Right(0)) {
      case (_, Left(e)) => Left(e.getMessage)
      case (Right(acc), Right(n)) => Right(acc + n)
    }

  def addLinesAndDetectRepetition[F[_]]: Pipe[F, Int, Int] = ints => {
    sealed trait Action
    case class Done(n: Int) extends Action
    case class Continue(acc: Int = 0, soFar: Set[Int] = Set()) extends Action

    ints.repeat.scan[Action](Continue()) {
      case (Continue(acc, pastFrequencies), n) =>
        val newFrequency = n + acc
        if (pastFrequencies.contains(newFrequency))
          Done(newFrequency)
        else
          Continue(newFrequency, pastFrequencies + newFrequency)
    }
    .collectFirst{
      case Done(n) => n
    }
  }

  def part1: IO[Unit] = for {
    _        <- Commons.putStrln("Part 1...")
    maybeRes <- addLines(readInts[IO]).compile.last

    res = maybeRes.fold("empty result".asLeft[Int])(identity)

    _        <- Commons.putStrln(s"Done. \n$res")
  } yield ()

  def part2: IO[Unit] = for {
    _   <- Commons.putStrln("Part 2...")
    ints <- IO.delay(readInts[IO])
    res <- (
              ints.collect {
                  case Right(n) => n
                } through addLinesAndDetectRepetition
              ).compile.last
    _        <- Commons.putStrln(s"Done. \n$res")
  } yield ()

  def run(args: List[String]): IO[ExitCode] = for {
    _        <- IO.delay(println("starting..."))
    _ <- part1
    _ <- part2
  } yield ExitCode.Success

}
