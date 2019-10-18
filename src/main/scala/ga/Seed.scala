package ga

import cats.data.State

final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)

  private def int(max: Int): Int = {
    if (max == 0) {
      0
    } else {
      (long.abs % (max + 1)).toInt
    }
  }
}

object Seed {
  def apply(init: Long = System.currentTimeMillis): Seed = new Seed(init)

  def nextInt(max: Int): State[Seed, Int] = State(seed => (seed.next, seed.int(max)))

  val nextLong: State[Seed, Long] = State(seed =>
    (seed.next, seed.long))

  val nextDouble: State[Seed, Double] = nextLong.map { i =>
    (i / Long.MaxValue.toDouble).abs
  }
  val nextBoolean: State[Seed, Boolean] = nextLong.map(_ > 0)

}
