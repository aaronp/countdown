package ga

import scala.util.Random

object WeightedRandom {

  /**
   * Given an index into a originalPopulation array of size, return a different index
   *
   * @param index
   * @param size
   * @return
   */
  def randomNeighborIndex(index: Int, size: Int, nextInt: Int => Int): Int = {
    val offset = nextInt(size)
    val newIndex = index + offset
    if (newIndex < 0) {

    }
    ???
  }

  /**
   * Given the index to some gene, return the indices which it should mate with
   * @param i
   * @param max
   * @return
   */
  def chooseMateIndices(i: Int, max: Int): Seq[Int] = {
    Random.nextDouble() match {
      case n if n < 0.6 => if (i >= max - 1) {
        Seq(i - 1)
      } else {
        Seq(i + 1)
      }
      case n if n < 0.8 => if (i >= max - 2) {
        Seq(i - 2)
      } else {
        Seq(i + 2)
      }
      case _ if i >= max - 1 => Seq(i - 1)
      case _ => Seq(i + 1)
    }
  }
  /**
   * group some value with a percentage of likelihood (e.g. ("foo" -> 0.7, "bar" -> 0.2, "unlikely" -> 0.1)
   *
   * The percentages must add up to 1.0 or an error is thrown.
   *
   * @param buckets a mapping of percentages (e.g. 0.7, 0.2, 0.1) paired with some value A
   * @tparam A
   * @return one of the 'A' values
   */
  def apply[A](randomDoubleValue: Double, buckets: Seq[(A, Double)]): A = {
    require(buckets.map(_._2).sum == 1.0, "bad buckets")
    require(randomDoubleValue <= 1.0, s"bad randomDoubleValue $randomDoubleValue")
    require(randomDoubleValue >= 0.0, s"bad randomDoubleValue $randomDoubleValue")
    val thresholds: Seq[(A, Double)] = buckets.sortBy(_._2).inits.toList.init.map { initValues =>
      val threshold = initValues.map(_._2).sum
      val head = initValues.last._1
      head -> threshold
    }
    thresholds.reverse.collectFirst {
      case (value, threshold) if randomDoubleValue <= threshold => value
    }.getOrElse(sys.error(s"bug: couldn't find first $randomDoubleValue in $thresholds"))
  }

}
