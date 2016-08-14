trait Connection {
  val depStation: Int
  val arrStation: Int
  val depTime: Int
  val arrTime: Int
}
case class BasicConnection(depStation: Int, arrStation: Int, depTime: Int, arrTime: Int) extends Connection
case class Query(depStation: Int, arrStation: Int, depTime: Int)

object Csa {
  val infinity = BasicConnection(0, 0, 0, Int.MaxValue)

  def makeConnection[C <: Connection](shortest: Map[Int, C], start: Int, end: Int): List[C] = {
    if (end == start)
      List()
    else
      shortest.get(end) match {
        case Some(connection) =>
          connection :: makeConnection(shortest, start, connection.depStation)
        case None =>
          throw new Exception("Connection not found in map!")
      }
  }

  def find[C <: Connection](connections: List[C], query: Query): List[C] = {
    var shortest = Map[Int, C]()
    for (conn <- connections) {
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
    }
    return makeConnection(shortest, query.depStation, query.arrStation).reverse
  }
}