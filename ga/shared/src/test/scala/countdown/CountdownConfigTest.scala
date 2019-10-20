package countdown

import ga.GeneticAlgo.Generation
import ga.{BaseSpec, Seed}

class CountdownConfigTest extends BaseSpec {

  "CountdownConfig.solve" should {
    "work" in {

      val settings = CountdownConfig.makeAlgoSettings(
        targetNumber = 12,
        inputNumbers = Set(1, 2, 3, 4, 5),
        maxPopulation = 100,
        mutationProbability = 0.001,
        maxGenerations = 200
      )

      val rnd = Seed(1234)

      def logGeneration(g: Generation[Equation]) = {

      }

      val countdownCfg = new CountdownConfig(
        settings = settings,
        rand = rnd,
        inputValues = Set(1, 2, 3, 4, 5),
        targetValue = 12,
        logGeneration,
        minEquationSize = 5,
        maxEquationSize = 6,
        None
      )
      val ok = countdownCfg.solve
      println(ok)
    }
  }
}
