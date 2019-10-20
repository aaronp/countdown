package countdown

import java.nio.file.Path

import ga.GeneticAlgo.Generation
import ga._

final case class CountdownConfig(
                                  settings: AlgoSettings[Equation],
                                  rand: Seed,
                                  inputValues: Set[Int],
                                  targetValue: Int,
                                  debug: Generation[Equation] => Unit,
                                  minEquationSize: Int,
                                  maxEquationSize: Int,
                                  writeSolution: Option[(Path, Int)]
                                ) {

  def initPopulation: (Seed, Seq[Equation]) = {
    Equation.populate(inputValues,
      minEquationSize,
      maxEquationSize,
      settings.maxPopulationSize,
      rand)
  }

  //  initialPopulation : IndexedSeq[Equation]
  object implicits {
    implicit val algoSettings = settings
  }

  def solve(): Option[Geneology[Equation]] = {
    val (seed, population) = initPopulation
    import implicits.algoSettings
    GeneticAlgo.solve(population, seed, debug)
  }
}

object CountdownConfig {


  def makeAlgoSettings(targetNumber: Int,
                       inputNumbers: Set[Int],
                       maxPopulation: Int,
                       mutationProbability: Double,
                       maxGenerations: Int): AlgoSettings[Equation] = {
    implicit val ordering = Equation.orderingForTarget(targetNumber)
    implicit val show = Equation.showForTarget(targetNumber)
    val builder =
      AlgoSettings[Equation](maxPopulationSize = maxPopulation,
        maxGenerations = maxGenerations) {
        case (rnd, mom, dad) =>
          //
          // our mating (combination) function for countdown
          //
          val len = mom.size.min(dad.size)
          val nextChild = Seed.nextInt(len - 1).map { splitAt =>
            mom.combineAt(dad, splitAt)
          }
          nextChild.run(rnd).value
      }

    builder
      .withSuccessCriteria(_.isSuccessful(targetNumber))
      .mutateEvery(mutationProbability) {
        case (rnd, equation) =>

          val mutateFunction = Seed.nextBoolean.flatMap {
            case true =>
              // flip a number or operation
              val rndInt = Seed.nextInt(equation.size - 1)
              rndInt.flatMap(equation.mutateAt(_, inputNumbers))
            case false =>
              // truncate the equation length
              val rndInt = Seed.nextInt(equation.size - 1)
              rndInt.map(equation.truncate)
          }

          mutateFunction.run(rnd).value
      }
  }
}
