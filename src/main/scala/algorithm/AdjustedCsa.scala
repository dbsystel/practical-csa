package algorithm

import gtfs.{Footpath, Trip, TripConnection}

import scala.annotation.tailrec

/**
  * A CSA implementation that respects transfer times for stations, footpaths, and train specific change times
  * @param connections sorted array of connections by departure time
  * @param transferTimes map of transfer times by stops
  * @param footpaths map of footpaths from station
  */
class AdjustedCsa[C <: TripConnection](
                                        val connections: Array[C],
                                        val transferTimes: Map[Int, Int],
                                        val footpaths: Map[Int, Iterable[Footpath]]
                                      ) {

  /**
    * Helper function to find the connection from an array of shortest connections
    * The function travels back to start
    * @param shortest a map of shortest connections to stops
    * @param start the start stop of the connection
    * @param end the stop traveled to
    * @return Some list of connection to take from last to first or None if connection not found
    */
  private def makeConnection[C <: Connection](shortest: Map[Int, C], start: Int, end: Int): Option[List[C]] = {
    if (end == start) Some(List()) else shortest.get(end) map { connection =>
      connection :: makeConnection(shortest, start, connection.depStation).get
    }
  }

  /**
    * Helper function to check if two connections connect
    * This function assumes the connections meet at a.arrStation / b.depStation without checking it!
    */
  private def connects(a: TripConnection, b: TripConnection): Boolean = {
    a.trip == b.trip || a.arrTime <= b.depTime - transferTimes(b.depStation)
  }

  /**
    * Simple binary lower bound search looking for lowest value that predicate holds true
    * @param predicate function that maps values to true or false
    * @param values by predicate sorted array of values
    * @return lowest index that the predicate holds true for
    */
  private def findLowerBound[T](predicate: T => Boolean, values: Array[T]): Int = {
    @tailrec
    def binarySearch(lower: Int, upper: Int): Int = {
      if (lower >= upper)
        lower
      else {
        val between = (upper - lower) / 2 + lower
        if (predicate(values(between))) binarySearch(lower, between) else binarySearch(between + 1, upper)
      }
    }
    binarySearch(0, values.length - 1)
  }

  def find(query: Query): Option[List[TripConnection]] = {
    val infinity = TripConnection(0, 0, 0, Int.MaxValue, Trip(0, 0, 0, "No Trip"))

    var shortest: Map[Int, TripConnection] = Map()

    def insert(e: C) = {
      val paths = footpaths(e.arrStation)

      paths foreach {
        case Footpath(fromStopId, toStopId, minutes) => {
          println("Found footpath")
          if (e.arrTime + minutes < shortest.getOrElse(toStopId, infinity).arrTime) {
            println("Improved with Footpath!")
            shortest += (toStopId -> TripConnection(
              fromStopId, toStopId, e.arrTime, e.arrTime + minutes, Trip(0, 0, 0, s"Footpath $minutes minutes")
            ))
          }
        }
      }

      shortest += (e.arrStation -> e)
    }

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
          case Some(current) => if (current.arrTime > conn.arrTime) insert(conn)
          case None => insert(conn)
        }
      }
      i += 1
    }
    makeConnection(shortest, query.depStation, query.arrStation) map { _.reverse }
  }
}

