package algorithm

import gtfs.{Footpath, Trip}

import scala.annotation.tailrec

class Domination[T](criteria: ((T, T) => Boolean)*) {
  private def check(x: T, y: T)(criterion: (T, T) => Boolean): Int = {
    if (criterion(x, y)) 1 else if (criterion(y, x)) -1 else 0
  }

  def dominates(x: T, y: T): Boolean = {
    (criteria exists { check(x, y)(_) == 1 }) && (criteria forall { check(x, y)(_) >= 0 })
  }
}

object Domination {
  def apply[T](criteria: ((T, T) => Boolean)*): Domination[T] = new Domination(criteria: _*)
}

class McCsa(connections: Array[TripConnection], transferTimes: Map[Int, Int], footpaths: Map[Int, Iterable[Footpath]]) {
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

  def find(query: Query): ParetoSet[List[Connection]] = {
    def changes(l: List[Connection]) = l.collect({
      case x: TripConnection => x
    }).foldLeft(Set[Int]())(_ + _.trip).size - 1

    val dom = Domination(
      (a: List[Connection], b: List[Connection]) => a.last.depTime > b.last.depTime,
      (a: List[Connection], b: List[Connection]) => a.head.arrTime < b.head.arrTime,
      (a: List[Connection], b: List[Connection]) => changes(a) < changes(b)
    )

    def connects(a: Connection, b: TripConnection): Boolean = a match {
      case x: TripConnection => x.trip == b.trip || x.arrTime <= b.depTime - transferTimes(b.depStation)
      case x: FootConnection => x.arrTime <= b.depTime
    }

    val emptySet = new ParetoSet[List[Connection]](dom.dominates)
    var shortest: Map[Int, ParetoSet[List[Connection]]] = Map() withDefaultValue emptySet

    def insert(c: List[Connection]): Unit = c match {
      case (x: TripConnection) :: xs =>
        val newParetoSet = shortest(x.arrStation) + c
        if (shortest(x.arrStation) != newParetoSet) {
          for {
            l <- footpaths.get(x.arrStation)
            p <- l
            betterFootpath = FootConnection(p.fromStopId, p.toStopId, x.arrTime, x.arrTime + p.minutes) :: c
          } {
            println("Inserting Footpath!")
            insert(betterFootpath)
          }
        }
        shortest += x.arrStation -> newParetoSet
      case x :: xs => shortest += x.arrStation -> (shortest(x.arrStation) + c)
    }

    def insertIterable(l: Iterable[List[Connection]]) = {
      if (l.nonEmpty) {
        val arrStation = l.head.head.arrStation
        shortest += arrStation -> (shortest(arrStation) ++ l)
      }
    }

    var i = findLowerBound((c: TripConnection) => c.depTime >= query.depTime, connections)
    var breakTime = Long.MaxValue
    while (i < connections.length && connections(i).depTime < breakTime) {
      val conn = connections(i)
      if (conn.depStation == query.depStation && query.depTime <= conn.depTime)
        insert(List(conn))
      else
        insertIterable(shortest(conn.depStation) filter { l => connects(l.head, conn) } map { conn :: _ })
      i += 1
      breakTime = shortest(query.arrStation).map(_.head.arrTime + 300).foldLeft(Long.MaxValue)(_ min _)
    }

    shortest(query.arrStation)
  }
}
