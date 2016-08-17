import java.time.LocalTime

case class Stop(id: Int, name: String)
case class StopTime(tripId: Int, arrivalTime: Long, departureTime: Long, stopId: Int, stopSequence: Int)
case class CalendarDate(serviceId: Int, date: Int, exceptionType: Int)
case class Trip(routeId: Int, serviceId: Int, id: Int, tripHeadsign: String)
case class Route(id: Int, shortName: String, longName: String)
