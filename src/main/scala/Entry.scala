object Entry {
  def printConnection(cons: List[TripConnection], data: GTFSData) = {
    def resolveStop: Int => String = data.stops(_).name
    def resolveRoute: Int => String =  data.routes(_).longName
    def resolveTime: Int => String = t => t / 3600 + ":" + (t % 3600) / 60

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

    val from = data.findStopByName("Frankfurt(Main)Hbf").get
    val to = data.findStopByName("Berlin Hbf").get

    val res = Csa.find(data.connections, Query(from.id, to.id, 10 * 60 * 60 + 30 * 60))

    printConnection(res, data)
  }
}
