package rest

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

import algorithm._
import gtfs._
import org.analogweb.core.Servers
import org.analogweb.scala.Analogweb

object Endpoint extends Analogweb {
  var data = GTFSData.empty

  /**
    * Concats following connections that belong to the same trip by omitting the stops in between:
    * A -> B :: B -> C => A -> C
    * This creates a pretty connection for the user
    * @param l A connection that should be taken by the user
    * @return List of real (not elementary) connections
    */
  private def denseConnection(l: List[TripConnection]): List[TripConnection] = l match {
    case Nil => Nil
    case x :: xs => denseConnection(xs) match {
      case y :: ys if x.trip == y.trip =>
        TripConnection(x.depStation, y.arrStation, x.depTime, y.arrTime, x.trip) :: ys
      case ys => x :: ys
    }
  }

  /**
    * Transforms trip connections to a JSON printable connection that can be delived by the API
    * @param tripConnection a TripConnection
    * @return a NiceConnection derived from the input parameter
    */
  private def makeNiceConnection(tripConnection: TripConnection): NiceConnection = {
    val departureStation = data.stops(tripConnection.depStation)
    val arrivalStation = data.stops(tripConnection.arrStation)
    val departureTime = GTFSData.epoch.plusMinutes(tripConnection.depTime).toString
    val arrivalTime = GTFSData.epoch.plusMinutes(tripConnection.arrTime).toString
    val name = tripConnection.trip.tripHeadsign + " on " +
      (data.routes.get(tripConnection.trip.routeId) map { _.longName } getOrElse "the woods")
    NiceConnection(departureStation, arrivalStation, departureTime, arrivalTime, name)
  }

  /**
    * Calculates the minutes since epoch
    * @return Time since epoch in minutes
    */
  private def now = GTFSData.epoch.until(LocalDateTime.now(), ChronoUnit.MINUTES).toInt

  def main(args: Array[String]): Unit = {
    val dir = if (args.length > 0) args(0) else scala.io.StdIn.readLine("GTFS location: ")
    println("Initializing GTFS feed, this might take a while...")
    data = GTFSData.fromDirPath(dir)

    println(s"Found ${data.stops.size} stops and ${data.connections.length} connections!")
    Servers.create(8080).run()
  }

  /**
    * Gets the time from optional query parameter or returns current time if parameter is None
    * @param par optional Query parameter
    * @return Time in minutes since epoch from now or given time string
    */
  def getTime(par: Option[String]): Int = {
    par map { s => GTFSData.epoch.until(LocalDateTime.parse(s), ChronoUnit.MINUTES).toInt } getOrElse now
  }

  get("/query/{from}/{to}") { implicit r =>
    val time = getTime(r.queryOption("time"))

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
    val time = getTime(r.queryOption("time"))

    val res = List(param("from"), param("to")) map { s => data.stops.get(s.toInt) } match {
      case Some(start) :: Some(destination) :: Nil =>
        BackwardsCSA.find(data.connections, data.connections sortWith { _.arrTime < _.arrTime }, Query(start.id, destination.id, time))
      case _ => None
    }

    res map {
      denseConnection(_) map makeNiceConnection
    } map asJson getOrElse BadRequest(asText("Could not find a connecting journey!"))
  }

  get("/stop/{id}") { implicit r =>
    data.stops.get(param("id").toInt) map asJson getOrElse BadRequest(asText("Could not find a stop with that ID."))
  }

  get("/adjusted/{from}/{to}") { implicit r =>
    val time = getTime(r.queryOption("time"))

    val res = List(param("from"), param("to")) map { s => data.stops.get(s.toInt) } match {
      case Some(start) :: Some(destination) :: Nil =>
        new AdjustedCsa(data.connections, data.transferTimes, data.footpaths).find(Query(start.id, destination.id, time))
      case _ => None
    }

    res map {
      denseConnection(_) map makeNiceConnection
    } map asJson getOrElse BadRequest(asText("Could not find a connecting journey!"))
  }

  get("/mc/{from}/{to}") { implicit r =>
  val time = getTime(r.queryOption("time"))

  val res = List(param("from"), param("to")) map { s => data.stops.get(s.toInt) } match {
    case Some(start) :: Some(destination) :: Nil =>
      Some(new McCsa(data.connections).find(Query(start.id, destination.id, time)))
    case _ => None
  }

  res map {
    _ map { _.reverse } map { denseConnection(_) map makeNiceConnection }
  } map asJson getOrElse BadRequest(asText("Could not find a connecting journey!"))
}
}

case class NiceConnection(from: Stop, to: Stop, departureTime: String, arrivalTime: String, name: String)
