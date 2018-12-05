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
object Day3 extends IOApp {

  implicit val blockingEC: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1))

  val path = "/Users/pierangelo.cecchetto/Documents/projects/scala/piero/advent-of-code-2018/input/day3-1.txt"

  case class Vertex(x: Int, y: Int)
  case class Size(w: Int, h: Int)
  case class Claim(nr: Int, topLeft: Vertex, size: Size) {
    def points: List[Vertex] = for {
      horiz <- (0 until size.w).toList
      vert  <- (0 until size.h).toList
    } yield Vertex(topLeft.x + horiz, topLeft.y + vert)

    def overlaps(other: Claim): Boolean = points.toSet.intersect(other.points.toSet).nonEmpty
  }

  object Claim {
    val regex = "#(\\d+)\\s@\\s(\\d+),(\\d+):\\s(\\d+)x(\\d+)".r
    def fromString(s: String): Option[Claim] = s match {
      case regex(nr, tlx, tly, w, h) => Some(Claim(nr.toInt, Vertex(tlx.toInt, tly.toInt), Size(w.toInt, h.toInt)))
      case _ =>
        println("couldn't extract claim")
        None
    }
  }

  def countMultipleClaims[F[_]: Sync]: Pipe[F, Claim, Map[Vertex, Int]] = claims =>
    claims
      .flatMap(claim => Stream.emits(claim.points).covary[F])
      .map(vtx => Map(vtx -> 1))
      .foldMonoid
      .lastOr(Map())

  def accumulateNonOverlapping[F[_]: Sync]: Pipe[F, Claim, Set[Claim]] = claims =>
    claims.fold[(Set[Claim], Set[Claim])]((Set(), Set())) {
      case ((nonOverlapping, acc), claim) =>
        if (!acc.exists(_.overlaps(claim) )) (nonOverlapping + claim, acc + claim)
        else (nonOverlapping diff acc.filter(_ overlaps claim), acc + claim)
    }.map(_._1)

  def part1[F[_] : Sync : ContextShift]: F[Int] =  for {
    map <- (Commons.readLines[F](path, blockingEC).map(Claim.fromString).collect { case Some(r) => r }
                  through countMultipleClaims).compile.last.map(_.getOrElse(Map()))
  } yield map.values.count(_ > 1)

  def part2[F[_] : Sync : ContextShift]: F[Set[Claim]] =  for {
    set <- (Commons.readLines[F](path, blockingEC).map(Claim.fromString).collect { case Some(r) => r }
//    set <- (Stream("#1 @ 1,3: 4x4", "#2 @ 3,1: 4x4", "#3 @ 5,5: 2x2").covary[F].map(Claim.fromString).collect { case Some(r) => r }
      through accumulateNonOverlapping).compile.last.map(_.getOrElse(Set()))
  } yield set

  override def run(args: List[String]): IO[ExitCode] = for {

    _ <- Commons.putStrln("Part 1...")
    resPart1 <- part1[IO]
    _ <- Commons.putStrln(s"Result = $resPart1")

    _ <- Commons.putStrln("Part 2...")
    resPart2 <- part2[IO]
    _ <- Commons.putStrln(s"Result = $resPart2")

  } yield ExitCode.Success


    //    Commons.putStrln(Claim(0, Vertex(3, 2), Size(5, 4)).points.toString()) >>
//    Commons.putStrln(Claim.fromString("#123 @ 3,2: 5x4").get.points.toString()) >>
//    IO.pure(ExitCode.Success)

}
