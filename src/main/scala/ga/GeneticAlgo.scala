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

  type Generation[A] = (Int, IndexedSeq[Geneology[A]])

  def solve[A: AlgoSettings](
      population: Seq[A],
      seed: Seed = Seed(),
      debug: Generation[A] => Unit): Option[Geneology[A]] = {
    implicit val s: Show[A] = AlgoSettings[A].show
    run(population.map(x => Origin(x)).toIndexedSeq, 0, seed, debug)
  }

  // 1) start w/ an initial population
  private def run[A: AlgoSettings](
      population: IndexedSeq[Geneology[A]],
      generation: Int,
      rand: Seed,
      debug: Generation[A] => Unit): Option[Geneology[A]] = {

    val settings = AlgoSettings[A]

    debug(generation, population)

    // 2) sort on fitness
    population match {
      // 3) if there's a solution, good times...
      case head +: _ if settings.success(head.value) =>
        Option(head)
      case _ if generation > settings.maxGenerations => None
      case sorted                                    =>
        // 4) reproduce
        val (nextSeed, newPopulation) =
          createNextGeneration(sorted, rand, generation)
        run(newPopulation, generation + 1, nextSeed, debug)
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
  def createNextGeneration[A: AlgoSettings](
      originalPopulation: IndexedSeq[Geneology[A]],
      initialSeed: Seed,
      generation: Int): (Seed, IndexedSeq[Geneology[A]]) = {
    val settings = AlgoSettings[A]
    import settings.implicits._
    val max = originalPopulation.size

    //
    // for each record, mate it with one or more other records (passing on our random seed to determine the output)
    //
    val (newSeed, newGeneration) = originalPopulation.zipWithIndex.foldLeft(
      initialSeed -> IndexedSeq.empty[Geneology[A]]) {
      case ((nextSeed, results), (mom, i)) =>
        //
        // find out who to randomly mate with
        //
        val (mateSeed, partnerIndices) =
          AlgoSettings.nextMateIndices(i, max).run(nextSeed).value
        require(!partnerIndices.contains(i))

        //
        // actually combine the 'mom' and 'dad' records
        //
        val (updatedSeed, offspring) =
          partnerIndices.foldLeft(mateSeed -> Seq.empty[Geneology[A]]) {
            case ((seed, children), partnerIndex) =>
              val dad: Geneology[A] = originalPopulation(partnerIndex)
              val (newSeed, child) =
                combineAndMutate(seed, mom, dad, generation, i)
              (newSeed, child +: children)
          }

        val newChildren = offspring ++: results
        (updatedSeed, newChildren)
    }

    //
    // survival of the fittest! Sort and cull ...
    //
    val nextGen = newGeneration
      .distinctBy(_.value)
      .sortBy(_.value)
      .take(settings.maxPopulationSize)
    (newSeed, nextGen)
  }

  /**
    * Do the mating/mutating of the two records
    *
    * @param random        our 'random' seed used to control randomness in our function for determining how to mate the two parents
    * @param mom           the parent record 1
    * @param dad           another parent record
    * @param generation    some detail for the geneology
    * @param childRecordId some detail for the geneology
    * @tparam A
    * @return
    */
  def combineAndMutate[A: AlgoSettings](
      random: Seed,
      mom: Geneology[A],
      dad: Geneology[A],
      generation: Int,
      childRecordId: Int): (Seed, Geneology[A]) = {
    val settings = AlgoSettings[A]
    import settings.implicits.showInstance

    val (combineSeed, child: A) = settings.combine(random, mom.value, dad.value)

    val original = Offspring[A](child, generation, childRecordId, mom, dad)
    // potentially mutate some records
    val (mutateSeed, changedOpt) =
      settings.mutate(combineSeed, child, generation, childRecordId)
    val childGeneology = changedOpt.fold(original: Geneology[A]) { mutated =>
      Mutation[A](mutated, original)
    }
    (mutateSeed, childGeneology)
  }
}
