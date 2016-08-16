trait Connection {
  val depStation: Int
  val arrStation: Int
  val depTime: Int
  val arrTime: Int
}
case class BasicConnection(depStation: Int, arrStation: Int, depTime: Int, arrTime: Int) extends Connection
case class Query(depStation: Int, arrStation: Int, depTime: Int)

object Csa {
  def makeConnection[C <: Connection](shortest: Map[Int, C], start: Int, end: Int): List[C] = {
    if (end == start) List() else shortest.get(end) match {
      case Some(connection) =>
        connection :: makeConnection(shortest, start, connection.depStation)
      case None =>
        throw new Exception("Connection not found in map!")
    }
  }

  def find[C <: Connection](connections: Array[C], query: Query): List[C] = {
    val infinity = BasicConnection(0, 0, 0, Int.MaxValue)

    var shortest: Map[Int, C] = Map[Int, C]()
    var i = 1
    // since this calculates one to one queries we can break when the connection departs later then EAT at target stop
    while (i < connections.length && shortest.getOrElse(query.arrStation, infinity).arrTime > connections(i).depTime) {
      val conn = connections(i)
      if (
        conn.depStation == query.depStation && query.depTime < conn.depTime ||
          shortest.getOrElse(conn.depStation, infinity).arrTime < conn.depTime
      ) {
        shortest.get(conn.arrStation) match {
          case Some(current) =>
            if (current.arrTime > conn.arrTime) shortest = shortest + (conn.arrStation -> conn)
          case None =>
            shortest = shortest + (conn.arrStation -> conn)
        }
      }
      i += 1
    }
    makeConnection(shortest, query.depStation, query.arrStation).reverse
  }
}
