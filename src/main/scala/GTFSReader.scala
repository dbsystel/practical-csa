class GTFSData(val stops: Map[Int, Stop], val connections: List[Connection]) {
  def findStopByName(name: String): Option[Stop] = stops find { _._2.name == name } map { _._2 }
}

object GTFSData {
  private def makeConnectionsFromStops(timedStops: List[StopTime]): List[Connection] = timedStops match {
    case _ :: Nil => List()
    case Nil => List()
    case from :: to :: rest =>
      Connection(from.stopId, to.stopId, from.departureTime, to.arrivalTime) :: makeConnectionsFromStops(to :: rest)
  }

  def fromDirPath(path: String): GTFSData = {
    val stopData = StopReader.read(path + "stops.txt")
    val calendarData = CalendarDateReader.read(path + "calendar_dates.txt")
    val stopTimeData = StopTimeReader.read(path + "stop_times.txt")
    val tripData = TripReader.read(path + "trips.txt").toList.groupBy(t => t.serviceId)

    val stops = stopData.foldLeft(Map[Int, Stop]())((p: Map[Int, Stop], n: Stop) => p + (n.id -> n))
    val stopTimes = stopTimeData.toList.groupBy(s => s.tripId)

    val todaysConnections = calendarData filter { _.date == 20161108 }

    println(todaysConnections.size)

    val connections = for (date <- todaysConnections) {
      val trips: List[Trip] = tripData.getOrElse(date.serviceId, List())

      trips foreach {
        trip => stopTimes.get(trip.tripId) match {
          case Some()
        }
      }
    }

    new GTFSData(stops, List())
  }

  def main(args: Array[String]): Unit = {
    val data = fromDirPath("D:\\Users\\hendrikniemann\\Documents\\gtfsdata\\fv\\")
    data.findStopByName("Frankfurt(Main)Hbf").foreach(println)
  }
}
