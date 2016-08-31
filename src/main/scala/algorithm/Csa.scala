package algorithm

case class BasicConnection(depStation: Int, arrStation: Int, depTime: Long, arrTime: Long) extends Connection

object Csa {
  private def makeConnection[C <: Connection](shortest: Map[Int, C], start: Int, end: Int): Option[List[C]] = {
    if (end == start) Some(List()) else shortest.get(end) map { connection =>
        connection :: makeConnection(shortest, start, connection.depStation).get
    }
  }

  def find[C <: Connection](connections: Array[C], query: Query): Option[List[C]] = {
    val infinity = BasicConnection(0, 0, 0, Int.MaxValue)

    var shortest: Map[Int, C] = Map[Int, C]()

    // we look up the earliest relevant connection with binary search
    var i = findLowerBound((c: C) => c.depTime >= query.depTime, connections)
    // since this calculates one to one queries we can break when the connection departs later then EAT at target stop
    while (i < connections.length && shortest.getOrElse(query.arrStation, infinity).arrTime > connections(i).depTime) {
      val conn = connections(i)
      if (
        conn.depStation == query.depStation && query.depTime <= conn.depTime ||
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
    makeConnection(shortest, query.depStation, query.arrStation) map { _.reverse }
  }
}
