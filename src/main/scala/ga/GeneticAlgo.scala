package ga

import cats.Show

/**
 * 1) start with an initial population, represented as Seq[A]
 * 2) sort on fitness
 * 3) if there is a solution, good times, else ...
 * 4) reproduce - "mate" each entry with another within a certain proximity.
 * we will end up with another Seq[A] with the offspring
 * 5) potentially mutate some records - again go from Seq[A] -> Seq[A]
 */
object GeneticAlgo {

  def apply[A: AlgoSettings](population: Seq[A]): Option[Geneology[A]] = {
    implicit val s: Show[A] = AlgoSettings[A].show
    run(population.map(x => Origin(x)).toIndexedSeq, 0, Seed())
  }

  // 1) start w/ an initial population
  private def run[A: AlgoSettings](population: IndexedSeq[Geneology[A]], generation: Int, rand : Seed): Option[Geneology[A]] = {

    val settings = AlgoSettings[A]

    println(population.mkString(s"Generation $generation:\n", "\n", "\n\n"))

    // 2) sort on fitness
    population match {
      // 3) if there's a solution, good times...
      case head +: _ if settings.success(head.value) =>
        Option(head)
      case _ if generation > settings.maxGenerations => None
      case sorted =>
        // 4) reproduce
        val newPopulation = createNextGeneration(sorted, rand, generation)
        run(newPopulation, generation + 1)
    }
  }

  /**
   * This is our application logic - take some existing population (represented as [[Geneology]] so as to carry on a history)
   *
   * @param originalPopulation
   * @param generation a generation value to use when creating the new elements
   * @tparam A
   * @return
   */
  def createNextGeneration[A: AlgoSettings](originalPopulation: IndexedSeq[Geneology[A]], initialSeed : Seed, generation: Int): (Seed, IndexedSeq[Geneology[A]]) = {
    val settings = AlgoSettings[A]
    import settings.implicits._
    val max = originalPopulation.size
    val newGeneration: IndexedSeq[Geneology[A]] = originalPopulation.zipWithIndex.flatMap {
      case (mom, i) =>
        // determine who to mate with for this entry
        val matindices = AlgoSettings.nextMateIndex(max).run(initialSeed).value
        val partnerIndices = settings.chooseMateIndices(i, max)
        require(!partnerIndices.contains(i))

        val children: Seq[Geneology[A]] = partnerIndices.map { partnerIndex =>
          val dad: Geneology[A] = originalPopulation(partnerIndex)
          val child = settings.semigroup.combine(mom.value, dad.value)

          // potentially mutate some records
          settings.mutate(child, generation, i) match {
            case Some(mutated) =>
              val original = Offspring(child, generation, i, mom, dad)
              Mutation(mutated, original)
            case None =>
              Offspring(child, generation, i, mom, dad)
          }
        }
        children
    }

    // survival of the fittest!
    val nextGen = newGeneration.sortBy(_.value).take(settings.maxPopulationSize)
    ???
  }
}