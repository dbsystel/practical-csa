import java.io.File

import kantan.codecs.Result.{Failure, Success}
import kantan.csv._
import kantan.csv.ops._

import scala.io.Source

// Case classes for GTFS file input
//case class StopTime(tripId: Int, arrivalTime: String, departureTime: String, stopId: Int, stopSequence: Int)

/**
  * This class is used to read data from a GTFS feed and convert it into elementary connections
  */
object DataGenerator {
  //implicit val stopTimesDecoder: RowDecoder[StopTimes] = RowDecoder.decoder(0, 1, 2, 3, 4)(StopTimes.apply)
  type StopTime = (Int, String, String, Int, Int)

  def parse(folder: String): List[Connection] = {
    val stopTimes = new File(folder + "stop_times.txt").asCsvReader[StopTime](',', true) collect {
      case Success(a) => a
    } toList

    List()
  }

  def main(args: Array[String]): Unit = {
    parse("C:\\Users\\Hendrik\\Documents\\Uni\\Bachelorarbeit\\testdata\\")
  }
}
