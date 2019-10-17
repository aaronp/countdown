package countdown

import cats.kernel.Semigroup
import countdown.Element.Equation

object Solver {

  def solve(targetNumber: Int, inputNumbers: Set[Int], seed: Seed = Seed()): Option[Geneology[Equation]] = {
    val (_, population: Seq[Equation]) = Element.populate(inputNumbers, 5, 8, 10, Seed())

    def mutate(eq: Equation) = eq

    implicit val equationSemigroup = Semigroup.instance[Equation] {
      case (a, b) =>
        val index = a.length / 2
        val result = a.take(index) ++ b.drop(index)
        result
    }

    def diff(eq: Equation) = Element.eval(eq).map(targetNumber - _)

    implicit val order = Ordering.by[Equation, Int] { eq =>
      // order by difference
      diff(eq).getOrElse(Int.MaxValue)
    }

    implicit val settings: AlgoSettings[Equation] = {
      AlgoSettings(maxPopulationSize = 100, mutate) { equation =>
        diff(equation).exists(_ == 0)
      }
    }
    GeneticAlgo(population)
  }

}
