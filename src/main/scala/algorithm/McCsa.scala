package algorithm

import gtfs.{Footpath, TripConnection}

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

  def find(query: Query): ParetoSet[List[TripConnection]] = {
    def changes(l: List[TripConnection]) = l.foldLeft(Set[Int]())({ _ + _.trip.id }).size - 1

    val dom = Domination(
      (a: List[TripConnection], b: List[TripConnection]) => a.last.depTime > b.last.depTime,
      (a: List[TripConnection], b: List[TripConnection]) => a.head.arrTime < b.head.arrTime,
      (a: List[TripConnection], b: List[TripConnection]) => changes(a) < changes(b)
    )

    def domination(a: List[TripConnection], b: List[TripConnection]): Boolean = dom.dominates(a, b)

    def connects(a: TripConnection, b: TripConnection) = {
      a.trip == b.trip || a.arrTime <= b.depTime - transferTimes(b.depStation)
    }

    val emptySet = new ParetoSet[List[TripConnection]](domination)
    var shortest: Map[Int, ParetoSet[List[TripConnection]]] = Map() withDefaultValue emptySet

    def insert(c: List[TripConnection]) = shortest += c.head.arrStation -> (shortest(c.head.arrStation) + c)

    def insertIterable(l: Iterable[List[TripConnection]]) = {
      if (l.nonEmpty) {
        val arrStation = l.head.head.arrStation
        shortest += arrStation -> (shortest(arrStation) ++ l)
      }
    }

    var i = findLowerBound((c: TripConnection) => c.depTime >= query.depTime, connections)
    while (i < connections.length) {
      val conn = connections(i)
      if (conn.depStation == query.depStation && query.depTime <= conn.depTime)
        insert(List(conn))
      else
        insertIterable(shortest(conn.depStation) filter { l => connects(l.head, conn) } map { conn :: _ })
      i += 1
    }

    shortest(query.arrStation)
  }
}
