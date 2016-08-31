import scala.annotation.tailrec

/**
  * Created by hendrikniemann on 31.08.2016.
  */
package object algorithm {
  /**
  * Simple binary lower bound search looking for lowest value that predicate holds true
  *
  * @param predicate function that maps values to true or false
  * @param values    by predicate sorted array of values
  * @return lowest index that the predicate holds true for
  */
  def findLowerBound[T](predicate: T => Boolean, values: Array[T]): Int = {
    @tailrec
    def binarySearch(lower: Int, upper: Int): Int = {
      if (lower >= upper)
        lower
      else {
        val between = (upper - lower) / 2 + lower
        if (predicate(values(between))) binarySearch(lower, between) else binarySearch(between + 1, upper)
      }
    }
    binarySearch(0, values.length - 1)
  }
}
