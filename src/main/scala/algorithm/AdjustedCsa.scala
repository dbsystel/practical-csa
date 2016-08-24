package algorithm

import gtfs.{Trip, TripConnection}

import scala.annotation.tailrec

class AdjustedCsa[C <: TripConnection](val connections: Array[C], val transferTimes: Map[Int, Int]) {
  private def makeConnection[C <: Connection](shortest: Map[Int, C], start: Int, end: Int): Option[List[C]] = {
    if (end == start) Some(List()) else shortest.get(end) map { connection =>
      connection :: makeConnection(shortest, start, connection.depStation).get
    }
  }

  private def connects(a: TripConnection, b: TripConnection): Boolean = {
    a.trip == b.trip || a.arrTime <= b.depTime - transferTimes(b.depStation)
  }

  private def findLowerBound[T](criterion: T => Boolean, values: Array[T]): Int = {
    @tailrec
    def binarySearch(lower: Int, upper: Int): Int = {
      if (lower >= upper)
        lower
      else {
        val between = (upper - lower) / 2 + lower
        if (criterion(values(between))) binarySearch(lower, between) else binarySearch(between + 1, upper)
      }
    }
    binarySearch(0, values.length - 1)
  }

  def find(query: Query): Option[List[C]] = {
    val infinity = TripConnection(0, 0, 0, Int.MaxValue, Trip(0, 0, 0, "No Trip"))

    var shortest: Map[Int, C] = Map[Int, C]()

    // we look up the earliest relevant connection with binary search
    var i = findLowerBound((c: C) => c.depTime >= query.depTime, connections)
    // since this calculates one to one queries we can break when the connection departs later then EAT at target stop
    while (i < connections.length && shortest.getOrElse(query.arrStation, infinity).arrTime > connections(i).depTime) {
      val conn = connections(i)
      if (
        conn.depStation == query.depStation && query.depTime <= conn.depTime ||
          connects(shortest.getOrElse(conn.depStation, infinity), conn)
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

