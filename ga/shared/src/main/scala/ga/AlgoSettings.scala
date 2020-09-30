package ga

import cats.Show
import cats.data.State
import ga.AlgoSettings.{Generation, Offset}

/**
 * @param maxPopulationSize the max population size to keep
 * @param maxGenerations    the maximum number of generations to run
 * @param success           a predicate to determine if 'A' satisfies the success criteria
 * @param mutate            a function which can optionally mutate a record
 * @param combine           a function which can combine two records given a random seed
 * @tparam A
 */
final case class AlgoSettings[A: Ordering : Show](
                                                   maxPopulationSize: Int,
                                                   maxGenerations: Int,
                                                   success: A => Boolean,
                                                   mutate: (Seed, A, Generation, Offset) => (Seed, Option[A]),
                                                   combine: (Seed, A, A) => (Seed, A)) {
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
  type Combine[A] = (Seed, A, A) => (Seed, A)

  def apply[A](implicit instance: AlgoSettings[A]): AlgoSettings[A] = instance

  def apply[A: Ordering : Show](maxPopulationSize: Int, maxGenerations: Int)(
    combine: Combine[A]): dsl.Builder1[A] = {
    dsl.Builder1(maxPopulationSize, maxGenerations, combine)
  }

  /**
   * Some builder syntax. You don't have to use this.
   */
  object dsl {

    case class Builder1[A: Ordering : Show] private(maxPopulationSize: Int,
                                                    maxGenerations: Int,
                                                    combine: Combine[A]) {

      /**
       * @param f the success predicate
       * @return a builder which can be used to fill in the mutation criteria
       */
      def withSuccessCriteria(f: A => Boolean) = Builder2(this, f)
    }

    final case class Builder2[A: Ordering : Show] private(
                                                           parent: Builder1[A],
                                                           success: A => Boolean) {

      /**
       * A simplified, partially applied mutation function
       *
       * @param mutateFrequency
       * @param f
       * @return
       */
      def mutateEvery(mutateFrequency: Double)(
        f: (Seed, A) => (Seed, A)): AlgoSettings[A] = {
        mutateUsing {
          case (rnd, a, _, _) =>
            val randomTransform =
              Seed.weightedBoolean(mutateFrequency).transform {
                case (seed, true) =>
                  val (newSeed, mutated) = f(seed, a)
                  (newSeed, Option(mutated))
                case (seed, false) => (seed, None)
              }
            randomTransform.run(rnd).value
        }
      }

      def mutateUsing(f: (Seed, A, Generation, Offset) => (Seed, Option[A]))
      : AlgoSettings[A] = {
        new AlgoSettings[A](
          maxPopulationSize = parent.maxPopulationSize,
          maxGenerations = parent.maxGenerations,
          success = success,
          mutate = f,
          combine = parent.combine
        )
      }
    }

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
  def nextMateIndices(forIndex: Int,
                      populationSize: PopulationSize): State[Seed, Seq[Int]] = {
    val chanceForMultipleMates: Double =
      (forIndex.toDouble / populationSize) match {
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

    val createMate: State[Seed, Offset] =
      nextMateIndex(forIndex, populationSize)
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
  private[ga] def nextMateIndex(forIndex: Int,
                                populationSize: PopulationSize): State[Seed, Int] = {
    def inRange(x: Int) = x != forIndex && x >= 0 && x < populationSize

    val range = Seed.nextDouble.map {
      case n if n < 0.4 =>
        n -> Seq(-1, 1).map(forIndex + _) // 40% chance to use one away
      case n if n < 0.5 =>
        n -> Seq(-2, 2).map(forIndex + _) // 10% chance to use two away
      case n if n < 0.6 =>
        n -> Seq(-3, 3).map(forIndex + _) // 10% chance to use two away
      case n => // 40% of just using some random mate
        val chosen = (populationSize * n).toInt
        val seq = if (chosen == populationSize) {
          Seq(populationSize, populationSize - 1, populationSize - 2)
        } else if (chosen == 0) {
          Seq(0, 1, 2)
        } else {
          Seq(chosen, chosen + 1, chosen - 1)
        }
        n -> seq
    }


    range.map {
      case (_, offsets) =>
        def safeDefault = (0 to populationSize).find(inRange).getOrElse(forIndex)
        offsets
          .find(inRange)
          .getOrElse(safeDefault)
    }
  }
}
