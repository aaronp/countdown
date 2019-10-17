package countdown

import cats.Semigroup
import countdown.AlgoSettings.{Generation, Offset, PartnerIndex, PopulationSize}

import scala.util.Random

/**
 *
 * @param maxPopulationSize
 * @param success
 * @tparam A
 */
final case class AlgoSettings[A: Semigroup : Ordering](maxPopulationSize: Int,
                                                       success: A => Boolean,
                                                       mutate: (A, Generation, Offset) => Option[A],
                                                       nextInt: Int => Int,
                                                       maxGenerations : Int,
                                                       chooseMateIndices: (PartnerIndex, PopulationSize) => Seq[PartnerIndex]
                                                      ) {
  def semigroup: Semigroup[A] = Semigroup[A]

  def ordering: Ordering[A] = Ordering[A]

  object implicits {
    implicit def semigroupInstance: Semigroup[A] = semigroup

    implicit def orderingInstance: Ordering[A] = ordering
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

  def mutateSometimes[A](f : A => A) = (value: A, g: Generation, o: Offset) => {
    if (Random.nextDouble() >= 0.98) {
      Option(f(value))
    } else {
      None
    }
  }

  def apply[A: Semigroup : Ordering](maxPopulationSize: Int, mutate : A => A)(success: A => Boolean): AlgoSettings[A] = {

    new AlgoSettings[A](maxPopulationSize, success, mutateSometimes(mutate), Random.nextInt, 100, WeightedRandom.chooseMateIndices)
  }
}
