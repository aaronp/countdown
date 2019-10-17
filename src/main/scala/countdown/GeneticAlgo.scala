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

  /**
   * Given an index into a sorted array of size, return a different index
   *
   * @param index
   * @param size
   * @return
   */
  def randomNeighborIndex(index: Int, size: Int, nextInt : Int => Int): Int = {
    val offset = nextInt(size)
    val newIndex = index + offset
    if (newIndex < 0) {

    }
  }

  sealed trait Geneology[A] {
    def value: A
  }

  case class Origin[A](override val value: A) extends Geneology[A]

  case class Offspring[A](override val value: A, mom: Geneology[A], dad: Geneology[A]) extends Geneology[A]


  // 1) start w/ an initial population
  def run[A: AlgoSettings](population: IndexedSeq[Geneology[A]], generation: Int): Option[Geneology[A]] = {
    val settings = AlgoSettings[A]
    //import settings._
    // 2) sort on fitness
    population.sorted match {
      // 3) if there's a solution, good times...
      case head +: _ if settings.success(head) =>
        Option(head)
      case sorted =>
        // 4) reproduce
        val max = sorted.size
        val newGeneration = sorted.zipWithIndex.map {
          case (mom, i) =>
            val partnerIndex = randomNeighborIndex(i, max)
            val dad: Geneology[A] = sorted(partnerIndex)
            val child = settings.semigroup.combine(mom.value, dad.value)

            // 5) potentially mutate some records
            val mutated = settings.mutate(child, generation, i)
            Offspring(mutated, mom, dad)
        }

        // survival of the fittest!
        newGeneration.take(settings.maxPopulationSize)
    }
  }

}
