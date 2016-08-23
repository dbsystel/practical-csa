package algorithm

import scala.annotation.tailrec

object BackwardsCSA {
  private def makeConnection[C <: Connection](shortest: Map[Int, C], start: Int, end: Int): Option[List[C]] = {
    if (end == start) Some(List()) else shortest.get(end) map { connection =>
      connection :: makeConnection(shortest, start, connection.arrStation).get
    }
  }

  private def findLowerBound[T](criterion: T => Boolean, values: Array[T]): Int = {
    @tailrec
    def binarySearch(lower: Int, upper: Int): Int = {
      if (lower >= upper)
        lower
      else {
        val between = (upper - lower) / 2 + lower
        if (criterion(values(between)))
          binarySearch(lower, between)
        else
          binarySearch(between + 1, upper)
      }
    }
    binarySearch(0, values.length - 1)
  }

  def find[C <: Connection](byDeparture: Array[C], byArrival: Array[C], query: Query): Option[List[C]] = {
    val infinity = BasicConnection(0, 0, 0, Int.MaxValue)

    var shortest: Map[Int, C] = Map[Int, C]()

    // we look up the earliest relevant connection with binary search
    var i = findLowerBound((c: C) => c.depTime >= query.depTime, byDeparture)
    // since this calculates one to one queries we can break when the connection departs later then EAT at target stop
    while (i < byDeparture.length && shortest.getOrElse(query.arrStation, infinity).arrTime > byDeparture(i).depTime) {
      val conn = byDeparture(i)
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

    // If we could not find an EAT for the target station we can exit here
    if (!shortest.isDefinedAt(query.arrStation)) return None

    val eat = shortest(query.arrStation).arrTime

    // now we scan backwards through the array to find the latest departing connection arriving at eat
    i = findLowerBound((c: C) => c.arrTime > eat, byArrival) - 1

    shortest = Map()

    while (byArrival(i).arrTime >= query.depTime) {
      val conn = byArrival(i)
      if (
        conn.arrStation == query.arrStation && eat >= conn.arrTime ||
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

    makeConnection(shortest, query.arrStation, query.depStation)
  }
}

