package gtfs

import java.io.{File, FileNotFoundException}

import kantan.codecs.ResourceIterator
import kantan.codecs.Result.Success
import kantan.csv.RowDecoder
import kantan.csv.ops._

abstract class AbstractReader[T] {
  protected def findFile(path: String): File = {
    val file = new File(path)
    if (!file.exists()) throw new FileNotFoundException(path) else file
  }

  protected implicit val decoder: RowDecoder[T]

  def read(file: String): ResourceIterator[T] = findFile(file) asCsvReader[T](',', true) collect {
    case Success(a) => a
  }
}

object StopReader extends AbstractReader[Stop] {
  implicit val decoder: RowDecoder[Stop] = RowDecoder.decoder(0, 1)(Stop.apply)
}

object StopTimeReader extends AbstractReader[StopTime] {
  private def timeStringToInt(str: String) = str.split(':') map { _.toInt } match {
    case Array(h, m, s) => h * 60 + m
    case _ => throw new IllegalArgumentException("Provided time string must have format hh:mm:ss!")
  }

  implicit val decoder: RowDecoder[StopTime] = RowDecoder.decoder(0, 1, 2, 3, 4)(
    (id: Int, arr: String, dep: String, sid: Int, seq: Int) =>
      StopTime(id, timeStringToInt(arr), timeStringToInt(dep), sid, seq)
  )
}

object CalendarDateReader extends AbstractReader[CalendarDate] {
  implicit val decoder: RowDecoder[CalendarDate] = RowDecoder.decoder(0, 1, 2)(CalendarDate.apply)
}

object TripReader extends  AbstractReader[Trip] {
  implicit val decoder: RowDecoder[Trip] = RowDecoder.decoder(0, 1, 2, 3)(Trip.apply)
}

object RouteReader extends AbstractReader[Route] {
  implicit val decoder: RowDecoder[Route] = RowDecoder.decoder(0, 1, 2)(Route.apply)
}

object TransferReader extends AbstractReader[Transfer] {
  implicit val decoder: RowDecoder[Transfer] = RowDecoder.decoder(0, 1, 2, 3, 4, 5)(
    (fromStop: Int, toStop: Int, transferType: Int, minTime: Int, fromTrip: String, toTrip: String) => {
      if (!fromTrip.isEmpty && !toTrip.isEmpty) TripTransfer(fromTrip.toInt, toTrip.toInt, minTime / 60)
      else if (fromTrip.isEmpty && toTrip.isEmpty && fromStop == toStop) MinimumTransferTime(fromStop, minTime / 60)
      else if (fromTrip.isEmpty && toTrip.isEmpty) Footpath(fromStop, toStop, minTime / 60)
      else UnknownTransfer
    }
  )
}
