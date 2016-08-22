package cli

import gtfs._
import algorithm._
import java.time.temporal.ChronoUnit
import java.time.{LocalDateTime, LocalTime}

object Entry {
  def printConnection(cons: List[TripConnection], data: GTFSData) = {
    def resolveStop: Int => String = data.stops(_).name
    def resolveRoute: Int => String =  data.routes(_).longName
    def resolveTime: Long => String = t => LocalTime.of((t / 60) % 24 toInt, t % 60 toInt).toString

    def denseConnection(l: List[TripConnection]): List[TripConnection] = l match {
      case Nil => Nil
      case x :: xs => denseConnection(xs) match {
        case y :: ys if x.trip == y.trip =>
          TripConnection(x.depStation, y.arrStation, x.depTime, y.arrTime, x.trip) :: ys
        case ys => x :: ys
      }
    }

    def connectionToString(connection: TripConnection) = {
      val fromString = resolveStop(connection.depStation) + " " + resolveTime(connection.depTime)
      val toString = resolveStop(connection.arrStation) + " " + resolveTime(connection.arrTime)

      s"${resolveRoute(connection.trip.routeId)}: $fromString => $toString"
    }

    println(s"Connection from ${resolveStop(cons.head.depStation)} to ${resolveStop(cons.last.arrStation)}")

    denseConnection(cons) map connectionToString foreach { println }
  }

  def main(args: Array[String]): Unit = {
    val data = GTFSData.fromDirPath("D:/Users/hendrikniemann/Documents/gtfsdata/fv/")

    val from = data.findStopByName("Mannheim Hbf").get
    val to = data.findStopByName("Marburg(Lahn)").get
    val now = LocalDateTime.of(2000, 1, 1, 0, 0).until(LocalDateTime.now(), ChronoUnit.MINUTES).toInt

    val res = Csa.find(data.connections, Query(from.id, to.id, now)).get

    printConnection(res, data)
  }
}
