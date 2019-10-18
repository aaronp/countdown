package countdown

import ga.{AlgoSettings, Geneology, GeneticAlgo, Seed}

object Solver {

  def solve(targetNumber: Int, inputNumbers: Set[Int], seed: Seed = Seed()): Option[Geneology[Equation]] = {
    val (_, population: Seq[Equation]) = Equation.populate(inputNumbers, 5, 8, 100, Seed())

    implicit val settings: AlgoSettings[Equation] = {
      AlgoSettings(maxPopulationSize = 1000, (_: Equation).mutate()) { equation =>
        equation.diff(targetNumber) == 0
      }
    }

    GeneticAlgo(population)
  }

}
