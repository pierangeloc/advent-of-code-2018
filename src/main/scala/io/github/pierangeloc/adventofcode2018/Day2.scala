package io.github.pierangeloc.adventofcode2018

import java.util.concurrent.Executors

import cats.effect.{ContextShift, ExitCode, IO, IOApp, Sync}
import cats.implicits._
import fs2._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}


/**
  *
  * advent-of-code-2018 - 2018-12-04
  * Created with ♥ in Amsterdam
  */
object Day2 extends IOApp {

  implicit val blockingEC: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1))

  val path = "input/day2-1.txt"

  def mulOccurrences(s: String): Set[Int] = s.toList
    .groupBy(identity)
    .mapValues(_.size)
    .filter {
      case (_, occurrences) => occurrences > 1
    }
    .values
    .toSet

  def count[F[_]](xs: Stream[F, String]) = xs
    .map(s => mulOccurrences(s).toList.map((_, 1)).toMap)
    .foldMonoid

  def delta(s1: String, s2: String): String = (s1.toList zip s2.toList).map {
    case (c1, c2) => if (c1 == c2) c1 else '_'
  }.mkString

  def part1[F[_]: Sync : ContextShift]: F[Int] = for {
    maybeRes <- count(Commons.readLines[F](path, blockingEC))
  .compile.last
    res      <- maybeRes.fold(Sync[F].raiseError[Int](new RuntimeException("Couldn't fold over all values"))){
      map => Sync[F].delay(map.values.product)
    }
  } yield res

  def pairwiseDistances[F[_]]: Pipe[F, String, String] = codes => for {
    c1 <- codes
    c2 <- codes
  } yield delta(c1, c2)

  def part2[F[_] : Sync : ContextShift]: F[String] = for {
    lines <- (Commons.readLines[F](path, blockingEC) through pairwiseDistances)
        .filter(_.filter(_ == '_').size == 1).head
        .compile.toList
    res   <- lines.headOption.fold(Sync[F].raiseError[String](new RuntimeException("Nothing was found"))) { s =>
      Sync[F].pure(s.filter(_ != '_'))
    }
  } yield res


  def run(args: List[String]): IO[ExitCode] = for {
    _        <- Commons.putStrln("Part 1...")
    resPart1 <- part1[IO]
    _        <- Commons.putStrln(s"Result = $resPart1")

    _        <- Commons.putStrln("Part 2...")
    resPart2 <- part2[IO]
    _        <- Commons.putStrln(s"Result = $resPart2")
  } yield ExitCode.Success

}
