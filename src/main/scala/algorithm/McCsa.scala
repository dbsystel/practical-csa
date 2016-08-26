package algorithm

import gtfs.TripConnection

class McCsa(connections: Array[TripConnection]) {
  def find(query: Query): ParetoSet[List[TripConnection]] = {
    def domination(a: List[TripConnection], b: List[TripConnection]): Boolean = {
      (a.last.depTime > b.last.depTime && a.head.arrTime <= b.head.arrTime) ||
        (a.head.arrTime < b.head.arrTime && a.last.depTime >= b.last.depTime)
    }

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
