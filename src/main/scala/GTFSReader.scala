case class TripConnection(
                           depStation: Int,
                           arrStation: Int,
                           depTime: Int,
                           arrTime: Int,
                           trip: Trip
                         ) extends Connection

class GTFSData(val stops: Map[Int, Stop], val connections: List[TripConnection], val routes: Map[Int, Route]) {
  def findStopByName(name: String): Option[Stop] = stops find { _._2.name == name } map { _._2 }
}

object GTFSData {
  private def makeConnectionsFromStops(trip: Trip)(timedStops: List[StopTime]): List[TripConnection] = timedStops match {
    case _ :: Nil => List()
    case Nil => List()
    case from :: to :: rest =>
      TripConnection(from.stopId, to.stopId, from.departureTime, to.arrivalTime, trip) :: makeConnectionsFromStops(trip)(to :: rest)
  }

  def fromDirPath(path: String): GTFSData = {
    val stopData = StopReader.read(path + "stops.txt")
    val calendarData = CalendarDateReader.read(path + "calendar_dates.txt")
    val stopTimeData = StopTimeReader.read(path + "stop_times.txt")
    val tripData = TripReader.read(path + "trips.txt").toList.groupBy(t => t.serviceId)
    val routeData = RouteReader.read(path + "routes.txt")

    val stops = stopData.foldLeft(Map[Int, Stop]())((p: Map[Int, Stop], n: Stop) => p + (n.id -> n))
    val routes = routeData.foldLeft(Map[Int, Route]())((p: Map[Int, Route], n: Route) => p + (n.id -> n))
    val stopTimes = stopTimeData.toList.groupBy(s => s.tripId)

    val todaysConnections = calendarData filter { _.date == 20161108 }

    val connections: List[TripConnection] = todaysConnections.toList flatMap {
      (operationDate: CalendarDate) => {
        // If a service operates on a date all trips of the service operate on this date
        // we then have to go through all of these trips and add their connections
        val trips: List[Trip] = tripData.getOrElse(operationDate.serviceId, List())

        trips flatMap {
          trip => stopTimes.get(trip.id) match {
            case Some(l) => makeConnectionsFromStops(trip)(l)
            case None => List()
          }
        }
      }
    }

    new GTFSData(stops, connections, routes)
  }

  def main(args: Array[String]): Unit = {
    val data = fromDirPath("C:/Users/Hendrik/Documents/Uni/Bachelorarbeit/testdata/")
    val ffm = data.findStopByName("Frankfurt(Main)Hbf").get
    val berlin = data.findStopByName("Berlin Hbf").get

    val res = Csa.find(data.connections, Query(ffm.id, berlin.id, 6000))

    val resolveStop: Int => String = data.stops(_).name
    val resolveRoute: Int => String =  data.routes(_).longName

    res map {
      c => s"${resolveStop(c.depStation)} => ${resolveStop(c.arrStation)} (${resolveRoute(c.trip.routeId)})"
    } foreach { println }
  }
}
