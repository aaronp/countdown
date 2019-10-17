package countdown

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
    run(population.map(Origin.apply).toIndexedSeq, 0)
  }

  // 1) start w/ an initial population
  private def run[A: AlgoSettings](population: IndexedSeq[Geneology[A]], generation: Int): Option[Geneology[A]] = {
    val settings = AlgoSettings[A]
    // 2) sort on fitness
    population match {
      // 3) if there's a solution, good times...
      case head +: _ if settings.success(head.value) =>
        Option(head)
      case _ if generation > settings.maxGenerations => None
      case sorted =>
        // 4) reproduce
        val max = sorted.size
        val newGeneration: IndexedSeq[Geneology[A]] = sorted.zipWithIndex.flatMap {
          case (mom, i) =>
            val partnerIndices = settings.chooseMateIndices(i, max)
            require(!partnerIndices.contains(i))

            val children = partnerIndices.map { partnerIndex =>
              val dad: Geneology[A] = sorted(partnerIndex)
              val child = settings.semigroup.combine(mom.value, dad.value)

              // 5) potentially mutate some records
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
        import settings.implicits._
        val newPopulation = newGeneration.sortBy(_.value).take(settings.maxPopulationSize)
        run(newPopulation, generation + 1)
    }
  }
}