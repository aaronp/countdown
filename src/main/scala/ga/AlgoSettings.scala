package ga

import cats.data.State
import cats.{Semigroup, Show}
import ga.AlgoSettings.{Generation, Offset}

import scala.util.Random

/**
 *
 * @param maxPopulationSize
 * @param success
 * @tparam A
 */
final case class AlgoSettings[A: Semigroup : Ordering : Show](maxPopulationSize: Int,
                                                              success: A => Boolean,
                                                              mutate: (Seed, A, Generation, Offset) => (Seed, Option[A]),
                                                              nextInt: Int => Int,
                                                              maxGenerations: Int,
                                                              combine : (Seed, A, A) => (Seed, A)
                                                             ) {

  def ordering: Ordering[A] = Ordering[A]

  def show: Show[A] = Show[A]

  object implicits {

    implicit def orderingInstance: Ordering[A] = ordering

    implicit def showInstance: Show[A] = show
  }

}

object AlgoSettings {

  /**
   * when choosing a gene at some population index i, this is the index of the next index to find
   */
  type OffsetLikelihood = (Set[Int], Double)

  type PopulationSize = Int
  type PartnerIndex = Int
  type Generation = Int
  type Offset = Int

  def apply[A](implicit instance: AlgoSettings[A]): AlgoSettings[A] = instance

  def mutateSometimes[A](f: A => A) = (value: A, g: Generation, o: Offset) => {
    if (Random.nextInt(1000) >= 998) {
      Option(f(value))
    } else {
      None
    }
  }

  def apply[A: Semigroup : Ordering : Show](maxPopulationSize: Int, mutate: A => A)(success: A => Boolean): AlgoSettings[A] = {

    new AlgoSettings[A](maxPopulationSize, success, mutateSometimes(mutate), Random.nextInt, 100)
  }

  /**
   * We assume indices near the 'top' (e.g. closes to zero, furthest from populationSize) are 'fitter',
   * so we give them more of an opportunity to produce multiple mates.
   *
   * If the index is in the top 30% of the population, we give it a 75% of having multiple mates
   * If the index is in the top 60% of the population, we give it a 50% of having multiple mates
   * Otherwise we give it a 25% of having multiple mates
   *
   * @param forIndex
   * @param populationSize
   */
  def nextMateIndices(forIndex: Int, populationSize: PopulationSize): State[Seed, Seq[Int]] = {
    val chanceForMultipleMates: Double = (forIndex.toDouble / populationSize) match {
      case n if n <= 0.3 => 0.75
      case n if n <= 0.6 => 0.5
      case _ => 0.25
    }

    val randomNumberOfMatesToHave: State[Seed, Int] = for {
      shouldHaveMultipleMates <- Seed.weightedBoolean(chanceForMultipleMates)
      numMates <- Seed.nextInt(if (shouldHaveMultipleMates) 2 else 0).map(_ + 1)
    } yield {
      numMates
    }

    val createMate: State[Seed, Offset] = nextMateIndex(forIndex, populationSize)
    randomNumberOfMatesToHave.transform {
      case (seed, n) =>
        (0 until n).foldLeft(seed -> Seq.empty[Int]) {
          case ((nextSeed, results), _) =>
            val (s, offset) = createMate.run(nextSeed).value
            (s, offset +: results)
        }
    }
  }

  /**
   * @param forIndex       the index to be paired against
   * @param populationSize the total size of the population
   * @return another index into a population which should be paired (mated) with the 'forIndex'
   */
  def nextMateIndex(forIndex: Int, populationSize: PopulationSize): State[Seed, Int] = {
    def inRange(x: Int) = x != forIndex && x >= 0 && x < populationSize

    val range: State[Seed, Seq[Int]] = Seed.nextDouble.map {
      case n if n < 0.4 => Seq(-1, 1).map(forIndex + _) // 40% chance to use one away
      case n if n < 0.5 => Seq(-2, 2).map(forIndex + _) // 10% chance to use two away
      case n if n < 0.6 => Seq(-3, 3).map(forIndex + _) // 10% chance to use two away
      case n => // 40% of just using some random mate
        val chosen = (populationSize * n).toInt
        Seq(chosen, chosen + 1, chosen - 1)
    }

    range.map { offsets =>
      offsets.find(inRange).getOrElse(sys.error(s"Bug: Couldn't find a valid mate in $offsets for $forIndex and $populationSize"))
    }
  }
}
