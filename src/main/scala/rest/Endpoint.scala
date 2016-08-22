package rest

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import algorithm.{BackwardsCSA, Csa, Query}
import gtfs._
import org.analogweb.core.Servers
import org.analogweb.scala.Analogweb

object Endpoint extends Analogweb {
  var data = GTFSData.empty

  private def denseConnection(l: List[TripConnection]): List[TripConnection] = l match {
    case Nil => Nil
    case x :: xs => denseConnection(xs) match {
      case y :: ys if x.trip == y.trip =>
        TripConnection(x.depStation, y.arrStation, x.depTime, y.arrTime, x.trip) :: ys
      case ys => x :: ys
    }
  }

  private def makeNiceConnection(tripConnection: TripConnection): NiceConnection = {
    val departureStation = data.stops(tripConnection.depStation)
    val arrivalStation = data.stops(tripConnection.arrStation)
    val departureTime = GTFSData.epoch.plusMinutes(tripConnection.depTime).toString
    val arrivalTime = GTFSData.epoch.plusMinutes(tripConnection.arrTime).toString
    val name = tripConnection.trip.tripHeadsign + " on " + data.routes(tripConnection.trip.routeId).longName

    NiceConnection(departureStation, arrivalStation, departureTime, arrivalTime, name)
  }

  private def now = GTFSData.epoch.until(LocalDateTime.now(), ChronoUnit.MINUTES).toInt

  def main(args: Array[String]): Unit = {
    val dir = if (args.length > 0) args(0) else scala.io.StdIn.readLine("GTFS location: ")
    println("Initializing GTFS feed, this might take a while...")
    data = GTFSData.fromDirPath(dir)

    println(s"Found ${data.stops.size} stops and ${data.connections.length} connections!")
    Servers.create(8080).run()
  }

  get("/query/{from}/{to}") { implicit r =>
    val time = r.queryOption("time") map {
      LocalDateTime.parse(_)
    } map {
      GTFSData.epoch.until(_, ChronoUnit.MINUTES).toInt
    } getOrElse now

    val from = data.stops.get(param("from").toInt)
    val to = data.stops.get(param("to").toInt)

    val res = List(param("from"), param("to")) map { s => data.stops.get(s.toInt) } match {
      case Some(start) :: Some(destination) :: Nil =>
        Csa.find(data.connections, Query(start.id, destination.id, time))
      case _ => None
    }

    res map {
      denseConnection(_) map makeNiceConnection
    } map asJson getOrElse BadRequest(asText("Could not find a connecting journey!"))
  }

  get("/backwards/{from}/{to}") { implicit r =>
    val time = r.queryOption("time") map {
      LocalDateTime.parse(_)
    } map {
      GTFSData.epoch.until(_, ChronoUnit.MINUTES).toInt
    } getOrElse now

    val from = data.stops.get(param("from").toInt)
    val to = data.stops.get(param("to").toInt)

    val res = List(param("from"), param("to")) map { s => data.stops.get(s.toInt) } match {
      case Some(start) :: Some(destination) :: Nil =>
        BackwardsCSA.find(data.connections, data.connections sortWith { _.arrTime > _.arrTime }, Query(start.id, destination.id, time))
      case _ => None
    }

    res map {
      denseConnection(_) map makeNiceConnection
    } map asJson getOrElse BadRequest(asText("Could not find a connecting journey!"))
  }

  get("/stop/{id}") { implicit r =>
    data.stops.get(param("id").toInt) map asJson getOrElse BadRequest(asText("Could not find a stop with that ID."))
  }
}

case class NiceConnection(from: Stop, to: Stop, departureTime: String, arrivalTime: String, name: String)
