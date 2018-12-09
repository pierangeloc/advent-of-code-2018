package io.github.pierangeloc.adventofcode2018

import java.util.concurrent.Executors

import cats.effect.{ContextShift, ExitCode, IO, IOApp, Sync}
import fs2._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

/**
  *
  * advent-of-code-2018 - 2018-12-09
  * Created with ♥ in Amsterdam
  */
object Day5 extends IOApp {

  implicit val blockingEC: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1))

  val path = "input/day5-1.txt"

  def react(a: Char, b: Char) = a.toLower == b.toLower && (a.isLower && b.isUpper || a.isUpper && b.isLower)
  def reaction(s: List[Char]): List[Char] = {
    s.foldLeft[List[Char]](Nil){
      case (Nil, newChar)    => newChar :: Nil
      case (h :: t, newChar) => if (react(newChar, h)) t else newChar :: h :: t
    }.reverse
  }

  def part1[F[_] : Sync : ContextShift]: Stream[F, List[Char]] =
    Commons.readChars[F](path, blockingEC)
      .fold[List[Char]](Nil) {
      case (soFar, char) => reaction(soFar :+ char)
    }

  def part2[F[_] : Sync : ContextShift]: Stream[F, (Char, String)] = for {
    excludingUnit <- Stream.emits(('a' to 'z').toList).covary[F]
    afterReaction <- Commons.readChars[F](path, blockingEC).filter(_.toLower != excludingUnit)
      .fold[List[Char]](Nil) {
      case (soFar, char) => reaction(soFar :+ char)
    }
  } yield excludingUnit -> afterReaction.mkString

  override def run(args: List[String]): IO[ExitCode] = for {
    _             <- Commons.putStrln("Part 1...")
    afterReaction <- part1[IO].compile.last.map(_.getOrElse(List()))
    _             <- Commons.putStrln(s"After reaction there are ${afterReaction.length} units left. ${afterReaction.mkString}")
    _             <- Commons.putStrln("Part 2...")
    afterReaction2 <- part2[IO].fold[Option[(Char, String)]](None) {
      case (None, (c, s)) => Some(c -> s)
      case (Some((c1, s1)), (c, s)) => if (s1.length < s.length) Some(c1 -> s1) else Some(c -> s)
    }.compile.last.map(_.flatten.get)
    _             <- Commons.putStrln(s"Shortest reaction is given removing ${afterReaction2._1} producing ${afterReaction2._2.length} units left. ${afterReaction2._2}")





  } yield ExitCode.Success

}

