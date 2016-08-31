package algorithm

class BackwardsCSA(connections: Array[TripConnection]) {
  private val byDeparture = connections
  private val byArrival = connections sortWith { _.arrTime < _.arrTime }

  private def makeConnection[C <: Connection](shortest: Map[Int, C], start: Int, end: Int): Option[List[C]] = {
    if (end == start) Some(List()) else shortest.get(end) map { connection =>
      connection :: makeConnection(shortest, start, connection.arrStation).get
    }
  }

  def find(depStation: Int, arrStation: Int, depTime: Long): Option[List[Connection]] = {
    val infinity = BasicConnection(0, 0, 0, Int.MaxValue)

    var shortest: Map[Int, Connection] = Map[Int, Connection]()

    // we look up the earliest relevant connection with binary search
    var i = findLowerBound((c: Connection) => c.depTime >= depTime, byDeparture)
    // since this calculates one to one queries we can break when the connection departs later then EAT at target stop
    while (i < byDeparture.length && shortest.getOrElse(arrStation, infinity).arrTime > byDeparture(i).depTime) {
      val conn = byDeparture(i)
      if (
        conn.depStation == depStation && depTime <= conn.depTime ||
          shortest.getOrElse(conn.depStation, infinity).arrTime < conn.depTime
      ) {
        shortest.get(conn.arrStation) match {
          case Some(current) =>
            if (current.arrTime > conn.arrTime) shortest += (conn.arrStation -> conn)
          case None =>
            shortest += (conn.arrStation -> conn)
        }
      }
      i += 1
    }

    // If we could not find an EAT for the target station we can exit here
    if (!shortest.isDefinedAt(arrStation)) return None

    val eat = shortest(arrStation).arrTime

    // now we scan backwards through the array to find the latest departing connection arriving at eat
    i = findLowerBound((c: Connection) => c.arrTime > eat, byArrival) - 1

    shortest = Map()

    while (i > 0 && byArrival(i).arrTime >= depTime) {
      val conn = byArrival(i)
      if (
        conn.arrStation == arrStation && eat >= conn.arrTime ||
          shortest.getOrElse(conn.arrStation, infinity).depTime > conn.arrTime
      ) {
        shortest.get(conn.depStation) match {
          case Some(current) =>
            if (current.depTime < conn.depTime) shortest += (conn.depStation -> conn)
          case None =>
            shortest += (conn.depStation -> conn)
        }
      }
      i -= 1
    }

    makeConnection(shortest, arrStation, depStation)
  }
}

