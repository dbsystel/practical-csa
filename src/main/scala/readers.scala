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
  implicit val decoder: RowDecoder[Stop] = RowDecoder.decoder(0, 1, 2, 3, 4)(Stop.apply)
}

object StopTimeReader extends AbstractReader[StopTime] {
  private def timeStringToInt(str: String) = str.split(':') map { _.toInt } match {
    case Array(h, m, s) => h * 3600 + m * 60 + s
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
