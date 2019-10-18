package ga

import cats.{Semigroup, Show}
import ga.AlgoSettings.{Generation, Offset, PartnerIndex, PopulationSize}

import scala.util.Random

/**
 *
 * @param maxPopulationSize
 * @param success
 * @tparam A
 */
final case class AlgoSettings[A: Semigroup : Ordering : Show](maxPopulationSize: Int,
                                                              success: A => Boolean,
                                                              mutate: (A, Generation, Offset) => Option[A],
                                                              nextInt: Int => Int,
                                                              maxGenerations: Int
                                                      ) {
  def semigroup: Semigroup[A] = Semigroup[A]

  def ordering: Ordering[A] = Ordering[A]
  def show: Show[A] = Show[A]

  object implicits {
    implicit def semigroupInstance: Semigroup[A] = semigroup
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

  def nextMateIndices(forIndex : Int, populationSize: PopulationSize) = {
    Seed.nextDouble.map {

      // choose a neighbor with a 60% chance
      case n if n < 0.6 => if (forIndex >= populationSize - 1) {
        Seq(forIndex - 1)
      } else {
        Seq(forIndex + 1)
      }
      case n if n < 0.8 => if (forIndex >= populationSize - 2) {
        Seq(forIndex - 2)
      } else {
        Seq(forIndex + 2)
      }
      case _ if forIndex >= populationSize - 1 => Seq(forIndex - 1)
      case _ => Seq(forIndex + 1)
    }
  }

}
