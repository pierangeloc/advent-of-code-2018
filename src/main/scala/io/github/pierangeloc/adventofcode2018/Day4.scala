package io.github.pierangeloc.adventofcode2018

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime}
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
object Day4 extends IOApp {

  implicit val blockingEC: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newFixedThreadPool(1))

  val path = "input/day4-1.txt"

  sealed trait Event {
    val timestamp: LocalDateTime
  }
  object Event {
    case class BeginsShift(timestamp: LocalDateTime, guardNr: Int) extends Event
    case class FallsAsleep(timestamp: LocalDateTime) extends Event
    case class WakesUp(timestamp: LocalDateTime) extends Event

    private val beginsShiftRegex = "\\[(\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2})\\]\\sGuard\\s#(\\d+)\\sbegins shift".r
    private val fallsAsleepRegex = "\\[(\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2})\\]\\sfalls asleep".r
    private val wakesUpRegex     = "\\[(\\d{4}-\\d{2}-\\d{2}\\s\\d{2}:\\d{2})\\]\\swakes up".r

    def fromString(s: String): Option[Event] = s match {
      case beginsShiftRegex(ts, guardNr) => BeginsShift(LocalDateTime.parse(ts, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")), guardNr.toInt).some
      case fallsAsleepRegex(ts)          => FallsAsleep(LocalDateTime.parse(ts, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))).some
      case wakesUpRegex(ts)              => WakesUp(LocalDateTime.parse(ts, DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm"))).some
      case _ =>
        println(s"Couldn't parse $s")
        None
    }
  }

  def part1[F[_] : Sync : ContextShift]: F[List[Event]] = for {
    events <- Commons.readLines[F](path, blockingEC).map(Event.fromString).collect { case Some(r) => r }.compile.toList
    sortedEvents = events.sortWith((e1, e2) => e1.timestamp.isBefore(e2.timestamp))
  } yield sortedEvents


  override def run(args: List[String]): IO[ExitCode] = for {

    _ <- Commons.putStrln("Part 1...")
    resPart1 <- part1[IO]
    _ <- Commons.putStrln(s"Result = $resPart1")

//    _ <- Commons.putStrln("Part 2...")
//    resPart2 <- part2[IO]
//    _ <- Commons.putStrln(s"Result = $resPart2")

  } yield ExitCode.Success


    //    Commons.putStrln(Claim(0, Vertex(3, 2), Size(5, 4)).points.toString()) >>
//    Commons.putStrln(Claim.fromString("#123 @ 3,2: 5x4").get.points.toString()) >>
//    IO.pure(ExitCode.Success)

}
