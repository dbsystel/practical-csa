case class Stop(id: Int, name: String, lat: Double, lon: Double, timezone: String)
case class StopTime(tripId: Int, arrivalTime: Int, departureTime: Int, stopId: Int, stopSequence: Int)
case class CalendarDate(serviceId: Int, date: Int, exceptionType: Int)
case class Trip(routeId: Int, serviceId: Int, tripId: Int, tripHeadsign: String)
