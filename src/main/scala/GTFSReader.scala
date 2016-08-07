import java.io.File

import kantan.codecs.Result.Success
import kantan.csv._
import kantan.csv.ops._

case class Stop(id: Int, name: String, lat: Double, lon: Double, timezone: String)
case class StopTime(tripId: Int, arrivalTime: Int, departureTime: Int, stopId: Int, stopSequence: Int)
case class CalendarDate(serviceId: Int, date: Int, exceptionType: Int)

class GTFSData(val stops: Map[Int, Stop], val connections: List[Connection]) {
  def findStopByName(name: String): Option[Stop] = stops find { _._2.name == name } map { _._2 }
}

object GTFSData {
  private def timeStringToInt(str: String) = str.split(':') map { _.toInt } match {
    case Array(h, m, s) => h * 3600 + m * 60 + s
    case _ => throw new IllegalArgumentException("Provided time string must have format hh:mm:ss!")
  }

  def fromDirPath(path: String): GTFSData = {
    implicit val stopDecoder: RowDecoder[Stop] = RowDecoder.decoder(0, 1, 2, 3, 4)(Stop.apply)
    implicit val stopTimeDecode: RowDecoder[StopTime] = RowDecoder.decoder(0, 1, 2, 3, 4)(
      (id: Int, arr: String, dep: String, sid: Int, seq: Int) =>
        StopTime(id, timeStringToInt(arr), timeStringToInt(dep), sid, seq)
    )
    implicit val calendarDateDecoder: RowDecoder[CalendarDate] = RowDecoder.decoder(0, 1, 2)(CalendarDate.apply)

    val stopData = new File(path + "stops.txt").asCsvReader[Stop](',', true) collect { case Success(a) => a }
    val calendarData = new File(path + "calendar_dates.txt").asCsvReader[CalendarDate](',', true) collect {
      case Success(a) => a
    }
    val stopTimeData = new File(path + "stop_times.txt").asCsvReader[StopTime](',', true) collect {
      case Success(a) => a
    }

    println(stopTimeData.size)
    val stops = stopData.foldLeft(Map[Int, Stop]())((p: Map[Int, Stop], n: Stop) => p + (n.id -> n))

    new GTFSData(stops, List())
  }

  def main(args: Array[String]): Unit = {
    val data = fromDirPath("C:\\Users\\Hendrik\\Documents\\Uni\\Bachelorarbeit\\testdata\\")

    data.findStopByName("Frankfurt(Main)Hbf").foreach(println)
  }
}
