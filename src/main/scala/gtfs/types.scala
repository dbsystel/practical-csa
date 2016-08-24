package gtfs

case class CalendarDate(serviceId: Int, date: Int, exceptionType: Int)
case class Route(id: Int, shortName: String, longName: String)
case class Stop(id: Int, name: String)
case class StopTime(tripId: Int, arrivalTime: Long, departureTime: Long, stopId: Int, stopSequence: Int)
case class Trip(routeId: Int, serviceId: Int, id: Int, tripHeadsign: String)
trait Transfer {
  val minutes: Int
}
case class MinimumTransferTime(stopId: Int, minutes: Int) extends Transfer
case class Footpath(fromStopId: Int, toStopId: Int, minutes: Int) extends Transfer
case class TripTransfer(fromTrip: Int, toTrip: Int, minutes: Int) extends Transfer
object UnknownTransfer extends Transfer {
  val minutes = 0
}
