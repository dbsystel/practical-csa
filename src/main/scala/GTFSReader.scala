case class TripConnection(
                           depStation: Int,
                           arrStation: Int,
                           depTime: Int,
                           arrTime: Int,
                           trip: Trip
                         ) extends Connection

class GTFSData(val stops: Map[Int, Stop], val connections: Array[TripConnection], val routes: Map[Int, Route]) {
  def findStopByName(name: String): Option[Stop] = stops find { _._2.name == name } map { _._2 }
}

object GTFSData {
  private def makeConnectionsFromStops(trip: Trip)(timedStops: List[StopTime]): List[TripConnection] = timedStops match {
    case Nil => Nil
    case _ :: Nil => Nil
    case from :: to :: rest =>
      TripConnection(from.stopId, to.stopId, from.departureTime, to.arrivalTime, trip) ::
        makeConnectionsFromStops(trip)(to :: rest)
  }

  def fromDirPath(path: String): GTFSData = {
    val stopData = StopReader.read(path + "stops.txt")
    val calendarData = CalendarDateReader.read(path + "calendar_dates.txt")
    val stopTimeData = StopTimeReader.read(path + "stop_times.txt")
    val routeData = RouteReader.read(path + "routes.txt")
    val tripData = TripReader.read(path + "trips.txt")

    // We would like to have the data in some different data structures for easier use
    // Maps with id as Key
    val stops = stopData.foldLeft(Map[Int, Stop]())((p: Map[Int, Stop], n: Stop) => p + (n.id -> n))
    val routes = routeData.foldLeft(Map[Int, Route]())((p: Map[Int, Route], n: Route) => p + (n.id -> n))

    // In Map[List] with foreign key
    val trips = tripData.toList.groupBy(t => t.serviceId)
    val stopTimes = stopTimeData.toList.groupBy(s => s.tripId)

    // Let's for now focus on one day
    val todaysConnections = calendarData filter { _.date == 20160815 }

    val connections: Array[TripConnection] = todaysConnections.toArray flatMap {
      (operationDate: CalendarDate) => {
        // If a service operates on a date all trips of the service operate on this date
        // we then have to go through all of these trips and add their connections
        val associatedTrips: List[Trip] = trips.getOrElse(operationDate.serviceId, Nil)

        associatedTrips flatMap {
          trip => stopTimes.get(trip.id) map makeConnectionsFromStops(trip) getOrElse Nil
        }
      }
    }

    new GTFSData(stops, connections.sortBy(c => c.depTime), routes)
  }
}
