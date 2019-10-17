package countdown

import cats.data.State

final case class Seed(long: Long) {
  def next = Seed(long * 6364136223846793005L + 1442695040888963407L)
}

object Seed {
  def apply(init: Long = System.currentTimeMillis): Seed = new Seed(init)

  val nextLong: State[Seed, Long] = State(seed =>
    (seed.next, seed.long))

  val nextDouble: State[Seed, Double] = nextLong.map { i =>
    i.toDouble / Long.MaxValue
  }
  val nextBoolean: State[Seed, Boolean] = nextLong.map(_ > 0)

}
