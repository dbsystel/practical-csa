import scala.io.Source

case class Connection(depStation: Int, arrStation: Int, depTime: Int, arrTime: Int)
case class Query(depStation: Int, arrStation: Int, depTime: Int)

object Csa {
  val infinity = Connection(0, 0, 0, Int.MaxValue)

  def lineToConnection(line: String): Connection = line split ' ' map { _.toInt } match {
    case Array(a1, a2, a3, a4) => Connection(a1, a2, a3, a4)
    case _ => throw new IllegalArgumentException("Provided line is no connection")
  }

  def lineToQuery(line: String): Query = line split ' ' map { _.toInt } match {
    case Array(a1, a2, a3) => Query(a1, a2, a3)
    case _ => throw new IllegalArgumentException("Provided line is no query")
  }

  def makeConnection(shortest: Map[Int, Connection], start: Int, end: Int): List[Connection] = {
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

  def find(connections: List[Connection], query: Query): List[Connection] = {
    var shortest = Map[Int, Connection]()
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
    return makeConnection(shortest, query.depStation, query.arrStation)
  }

  def main(args: Array[String]): Unit = {
    val lines = Source.fromInputStream(System.in).getLines().toArray
    val connections = lines takeWhile { !_.isEmpty() } map lineToConnection
    val query = lineToQuery(lines.last)

    val Query(depStation, arrStation, _) = query

    println(s"Query von $depStation nach $arrStation:")

    find(connections.toList, query).reverse foreach println
  }
}