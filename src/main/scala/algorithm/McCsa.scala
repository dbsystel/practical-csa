package algorithm

import gtfs.TripConnection

class Domination[T](criteria: ((T, T) => Boolean)*) {
  private def check(x: T, y: T)(criterion: (T, T) => Boolean): Int = {
    if (criterion(x, y)) 1 else if (criterion(y, x)) -1 else 0
  }

  def dominates(x: T, y: T): Boolean = {
    (criteria count { check(x, y)(_) == 1 }) > 0 && (criteria forall { check(x, y)(_) >= 0 })
  }
}

object Domination {
  def apply[T](criteria: ((T, T) => Boolean)*): Domination[T] = new Domination(criteria: _*)
}

class McCsa(connections: Array[TripConnection]) {
  def find(query: Query): ParetoSet[List[TripConnection]] = {
    val dom = Domination(
      (a: List[TripConnection], b: List[TripConnection]) => a.last.depTime > b.last.depTime,
      (a: List[TripConnection], b: List[TripConnection]) => a.head.arrTime < b.head.arrTime
    )
    
    def domination(a: List[TripConnection], b: List[TripConnection]): Boolean = dom.dominates(a, b)

    def connects(a: TripConnection, b: TripConnection) = {
      a.arrTime <= b.depTime
    }

    var shortest: Map[Int, ParetoSet[List[TripConnection]]] = Map() withDefaultValue new ParetoSet[List[TripConnection]](domination)

    def insert(c: List[TripConnection]) = shortest += c.head.arrStation -> (shortest(c.head.arrStation) + c)

    def insertIterable(l: Iterable[List[TripConnection]]) = {
      if (l.nonEmpty) {
        val arrStation = l.head.head.arrStation
        shortest += arrStation -> (shortest(arrStation) ++ l)
      }
    }

    var i = 0
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
