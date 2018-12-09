package io.github.pierangeloc.adventofcode2018

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, LocalTime}
import java.util.concurrent.Executors

import cats.effect.{ContextShift, ExitCode, IO, IOApp, Sync}
import cats.implicits._
import mouse.all._
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
    case class EndsShift(timestamp: LocalDateTime) extends Event

    object EndsShift {
      val endOfTime = EndsShift(LocalDateTime.of(999999999, 12, 31, 23, 59,59))
    }

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

  sealed trait GuardState {
    val guardNr: Int
    val timestamp: LocalDateTime
  }
  object GuardState {

    case class Awake(timestamp: LocalDateTime, guardNr: Int) extends GuardState

    case class Asleep(timestamp: LocalDateTime, guardNr: Int) extends GuardState

    def fromEventsWithGuardNr(events: List[(Int, Event)]): Stream[Pure, GuardState] = {
      def fromBeginShift(timestamp: LocalDateTime, guardNr: Int): Stream[Pure, GuardState] =
        Stream.iterate(GuardState.Awake(timestamp, guardNr))(awake => awake.copy(timestamp = awake.timestamp.plusMinutes(1)))

      def fromFallsAsleep(timestamp: LocalDateTime, guardNr: Int): Stream[Pure, GuardState] =
        Stream.iterate(GuardState.Asleep(timestamp, guardNr))(asleep => asleep.copy(timestamp = asleep.timestamp.plusMinutes(1)))

      def fromWakesUp(timestamp: LocalDateTime, guardNr: Int): Stream[Pure, GuardState] =
        Stream.iterate(GuardState.Awake(timestamp, guardNr))(awake => awake.copy(timestamp = awake.timestamp.plusMinutes(1)))

      def isBefore(toEvent: Event)(gs: GuardState): Boolean = gs.timestamp.isBefore(toEvent.timestamp) &&
        gs.timestamp.toLocalTime.compareTo(LocalTime.of(1, 0)) < 0 &&
        (gs.timestamp.toLocalTime.compareTo(LocalTime.of(0, 0)) >= 0)

      Stream.emits(events.zip(events.tail :+ (events.last._1 -> Event.EndsShift.endOfTime))).flatMap {
                case ((guardNr, Event.BeginsShift(timestamp, _)), toEvent) =>
                  fromBeginShift(timestamp, guardNr).takeWhile { isBefore(toEvent._2) }
                    case ((guardNr, Event.FallsAsleep(timestamp)), toEvent) =>
                  fromFallsAsleep(timestamp, guardNr).takeWhile { isBefore(toEvent._2) }
                case ((guardNr, Event.WakesUp(timestamp)), toEvent) =>
                  fromWakesUp(timestamp, guardNr).takeWhile { isBefore(toEvent._2) }
                case e =>
                  Stream.empty
      }
    }
  }

  def guardStatesDistribution[F[_] : Sync : ContextShift]: F[List[GuardState]] = for {
    events <- Commons.readLines[F](path, blockingEC).map(Event.fromString).collect { case Some(r) => r }.compile.toList

    sortedEvents      = events.sortWith((e1, e2) => e1.timestamp.isBefore(e2.timestamp))
    eventsWithGuardNr = sortedEvents.foldLeft(List[(Int, Event)]()) {
      case (Nil, e @ Event.BeginsShift(_, guardNr))            => (guardNr, e) :: Nil
      case (h :: t, e @ Event.BeginsShift(timestamp, guardNr)) => (guardNr, e) :: (h._1, Event.EndsShift(timestamp)) :: h :: t
      case (eventsWithGuard, e)                                => (eventsWithGuard.head._1, e) :: eventsWithGuard
    }.reverse
    guardStates       = GuardState.fromEventsWithGuardNr(eventsWithGuardNr).toList
  } yield guardStates

  def mostFrequentValue[A](as: List[A]): (A, Int) = as.groupBy(identity).mapValues(_.length).toList.maxBy(_._2)

  def part1(guardStates: List[GuardState]): (Int, Int, Int) = {
    val guardMostAsleep = guardStates.filter {
      case GuardState.Asleep(_, _) => true
      case _ => false
    }.groupBy(_.guardNr).mapValues(_.map(_ => 1).sum).toList.maxBy(_._2)._1
    val minuteMostAsleep = guardStates.collect {
      case GuardState.Asleep(ts, guardNr) if guardNr == guardMostAsleep => ts.toLocalTime.getMinute
    } |> mostFrequentValue

    (guardMostAsleep, minuteMostAsleep._1, guardMostAsleep * minuteMostAsleep._1)
  }

  def part2(guardStates: List[GuardState]): (Int, Int, Int) = {
    val mostAsleepGuardByMinute: Map[Int, (Int, Int)] = guardStates.filter {
      case GuardState.Asleep(_, _) => true
      case _ => false
    }.groupBy(_.timestamp.toLocalTime.getMinute).mapValues(_.map(_.guardNr) |> mostFrequentValue)
    val (minute, (guardNr, _)) = mostAsleepGuardByMinute.maxBy {
      case (sampleMinute, (mostAsleepGuard, asleepNr)) => asleepNr
    }
    (minute, guardNr, minute * guardNr)
  }

  override def run(args: List[String]): IO[ExitCode] = for {

    _           <- Commons.putStrln("Part 1...")
    guardStates <- guardStatesDistribution[IO]
    resPart1    = part1(guardStates)
    _ <- Commons.putStrln(s"""Guard most asleep = ${resPart1._1}
      Minute most asleep = ${resPart1._2}
      product = ${resPart1._3}""")

    _           <- Commons.putStrln("Part 1...")

    resPart2    = part2(guardStates)
    _ <- Commons.putStrln(s"""Minute where guard is most asleep = ${resPart2._1}
      Guard most asleep = ${resPart2._2}
      product = ${resPart2._3}""")

    //    _ <- Commons.putStrln("Part 2...")
//    resPart2 <- part2[IO]
//    _ <- Commons.putStrln(s"Result = $resPart2")

  } yield ExitCode.Success


    //    Commons.putStrln(Claim(0, Vertex(3, 2), Size(5, 4)).points.toString()) >>
//    Commons.putStrln(Claim.fromString("#123 @ 3,2: 5x4").get.points.toString()) >>
//    IO.pure(ExitCode.Success)

}
