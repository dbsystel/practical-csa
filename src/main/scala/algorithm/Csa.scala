package algorithm

import scala.annotation.tailrec

object Csa {
  def makeConnection[C <: Connection](shortest: Map[Int, C], start: Int, end: Int): List[C] = {
    if (end == start) List() else shortest.get(end) match {
      case Some(connection) =>
        connection :: makeConnection(shortest, start, connection.depStation)
      case None =>
        throw new Exception("Connection not found in map!")
    }
  }

  private def findStart[C <: Connection](connections: Array[C], startAt: Long): Int = {
    @tailrec
    def binarySearch(lower: Int, upper: Int): Int = {
      if (upper - lower < 2)
        lower
      else {
        val between = (upper - lower) / 2 + lower
        if (connections(between).depTime < startAt)
          binarySearch(between, upper)
        else
          binarySearch(lower, between)
      }
    }
    binarySearch(0, connections.length - 1)
  }

  def find[C <: Connection](connections: Array[C], query: Query): List[C] = {
    val infinity = BasicConnection(0, 0, 0, Int.MaxValue)

    var shortest: Map[Int, C] = Map[Int, C]()

    // we look up the earliest relevant connection with binary search
    var i = findStart(connections, query.depTime)
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
