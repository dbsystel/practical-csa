package algorithm

import gtfs.{Footpath, TripTransfer}

class Domination[T](criteria: ((T, T) => Boolean)*) extends ((T, T) => Boolean) {
  def apply(x: T, y: T): Boolean = (criteria exists { _(x, y) }) && (criteria forall { !_(y, x) })
}

class McCsa(
             connections: Array[TripConnection],
             transferTimes: Map[Int, Int],
             footpaths: Map[Int, Iterable[Footpath]],
             tripTransfers: Map[Int, Iterable[TripTransfer]]
           ) {
  def find(startStation: Int, targetStation: Int, startTime: Long): ParetoSet[List[Connection]] = {
    def changes(l: List[Connection]) = l.collect({
      case x: TripConnection => x
    }).foldLeft(Set[Int]())(_ + _.trip).size - 1

    def earliestConnection(c: List[Connection]): Long = c.head match {
      case FootConnection(_, _, _, arrTime) => arrTime
      case TripConnection(_, arrStation, _, arrTime, _) => transferTimes(arrStation) + arrTime
    }

    def minTrip(tid: Int): Int = tripTransfers.get(tid).
      map(_.map(_.minutes).min).
      getOrElse(Int.MaxValue)

    def nextTrip(c: List[Connection]): Long = c.head match {
      case FootConnection(_, _, _, arrTime) => arrTime
      case TripConnection(_, arrStation, _, arrTime, tripid) =>
        arrTime + (transferTimes(arrStation) min minTrip(tripid))
    }

    def connects(a: Connection, b: TripConnection): Boolean = a match {
      case x: TripConnection =>
        val tripTransfer = tripTransfers.get(x.trip) flatMap { _ find { _.toTrip == b.trip } }
        (tripTransfer.isDefined && x.arrTime <= b.depTime - tripTransfer.get.minutes) ||
        x.trip == b.trip || x.arrTime <= b.depTime - transferTimes(b.depStation)
      case x: FootConnection => x.arrTime <= b.depTime
    }

    val dom = new Domination(
      (a: List[Connection], b: List[Connection]) => a.last.depTime > b.last.depTime,
      (a: List[Connection], b: List[Connection]) => earliestConnection(a) < earliestConnection(b),
      (a: List[Connection], b: List[Connection]) => changes(a) < changes(b),
      (a: List[Connection], b: List[Connection]) => !(earliestConnection(b) < nextTrip(a))
    )

    val strictDom = new Domination(
      (a: List[Connection], b: List[Connection]) => a.last.depTime > b.last.depTime,
      (a: List[Connection], b: List[Connection]) => a.head.arrTime < b.head.arrTime,
      (a: List[Connection], b: List[Connection]) => changes(a) < changes(b)
    )

    val emptySet = new ParetoSet[List[Connection]](dom)
    val strictSet = new ParetoSet[List[Connection]](strictDom)
    var shortest: Map[Int, ParetoSet[List[Connection]]] = Map(targetStation -> strictSet) withDefaultValue emptySet

    def insert(c: List[Connection]): Unit = c match {
      case (x: TripConnection) :: xs =>
        val newParetoSet = shortest(x.arrStation) + c
        if (shortest(x.arrStation) != newParetoSet) {
          for {
            l <- footpaths.get(x.arrStation)
            p <- l
            betterFootpath = FootConnection(p.fromStopId, p.toStopId, x.arrTime, x.arrTime + p.minutes) :: c
          } insert(betterFootpath)
        }
        shortest += x.arrStation -> newParetoSet
      case x :: xs => shortest += x.arrStation -> (shortest(x.arrStation) + c)
    }

    var i = findLowerBound((c: TripConnection) => c.depTime >= startTime, connections)
    var breakTime = Long.MaxValue
    while (i < connections.length && connections(i).depTime < breakTime) {
      val conn = connections(i)
      if (conn.depStation == startStation && startTime <= conn.depTime)
        insert(List(conn))
      else {
        shortest(conn.depStation) filter { l => connects(l.head, conn) } map { conn :: _ } foreach insert

        footpaths(startStation) find { p =>
          p.toStopId == conn.depStation && startTime + p.minutes < conn.depTime
        } foreach { p =>
          insert(List(conn, FootConnection(p.fromStopId, p.toStopId, conn.depTime - p.minutes, conn.depTime)))
        }
      }
      i += 1
      breakTime = shortest(targetStation).map(_.head.arrTime + 300).foldLeft(Long.MaxValue)(_ min _)
    }

    shortest(targetStation)
  }
}
