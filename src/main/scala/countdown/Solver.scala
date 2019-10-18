package countdown

import ga.{AlgoSettings, GeneticAlgo, Seed}

object Solver {

  def solve(targetNumber: Int, inputNumbers: Set[Int], seed: Seed = Seed()) = {
    val (nextSeed, population: Seq[Equation]) = Equation.populate(inputNumbers, 5, 8, 100, seed)

    // we need to know the target to know how to order
    implicit val ordering = Equation.orderingForTarget(targetNumber)

    // create some settings, describing how to combine and mutate our equations
    implicit val settings: AlgoSettings[Equation] = {
      val builder = AlgoSettings[Equation](maxPopulationSize = 200, maxGenerations = 100) {
        case (rnd, mom, dad) =>
          val len = mom.size.min(dad.size)
          val nextChild = Seed.nextInt(len - 1).map(1 + _).map { splitAt =>
            mom.combineAt(dad, splitAt)
          }
          nextChild.run(rnd).value
      }

      builder.withSuccessCriteria(_.eval == targetNumber).mutateEvery(0.01) {
        case (rnd, equation) =>
          val mutate = Seed.nextInt(equation.size - 1).flatMap { index =>
            equation.mutateAt(index, inputNumbers)
          }
          mutate.run(rnd).value
      }
    }

    GeneticAlgo.solve(population, nextSeed)
  }

}
