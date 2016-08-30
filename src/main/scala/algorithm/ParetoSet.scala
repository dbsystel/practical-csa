package algorithm

class ParetoSet[T](dominator: (T, T) => Boolean, items: Set[T] = Set[T]()) extends Iterable[T] {
  def +(item: T): ParetoSet[T] = {
    if (items exists { dominator(_, item) }) this
    else new ParetoSet(dominator, items -- (items filter { dominator(item, _) }) + item)
  }

  def ++(iterable: Iterable[T]): ParetoSet[T] = iterable.foldLeft[ParetoSet[T]](this)(_ + _)

  override def iterator: Iterator[T] = items.iterator
}
