package algorithm

import gtfs.Trip

/**
  * Created by hendrikniemann on 17.08.2016.
  */
trait Connection {
  val depStation: Int
  val arrStation: Int
  val depTime: Long
  val arrTime: Long
}

case class TripConnection(
                           depStation: Int,
                           arrStation: Int,
                           depTime: Long,
                           arrTime: Long,
                           trip: Int
                         ) extends Connection

case class FootConnection(
                           depStation: Int,
                           arrStation: Int,
                           depTime: Long,
                           arrTime: Long
                         ) extends Connection
