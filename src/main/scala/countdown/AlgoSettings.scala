package countdown

import cats.Semigroup
import countdown.AlgoSettings.{Generation, Offset}

import scala.util.Random

/**
 *
 * @param maxPopulationSize
 * @param success
 * @tparam A
 */
final case class AlgoSettings[A: Semigroup : Ordering](maxPopulationSize: Int, success: A => Boolean, mutate: (A, Generation, Offset) => A, nextInt : Int => Int) {
  implicit def semigroup: Semigroup[A] = Semigroup[A]

  implicit def ordering: Ordering[A] = Ordering[A]
}

object AlgoSettings {

  type Generation = Int
  type Offset = Int

  def apply[A](implicit instance: AlgoSettings[A]): AlgoSettings[A] = instance

  def neverMutate[A] = (value: A, g: Generation, o: Offset) => value

  def apply[A: Semigroup : Ordering](maxPopulationSize: Int)(success: A => Boolean): AlgoSettings[A] = {
    new AlgoSettings[A](maxPopulationSize, success, neverMutate, Random.nextInt)
  }
}
