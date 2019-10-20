package countdown

import java.nio.file.{Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}
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
    val opt = GeneticAlgo.solve(population, seed, debug)

    for {
      (dir, maxNodes) <- writeSolution
      soln <- opt
    } {
      implicit val show = Equation.showForTarget(targetValue)
      HtmlRenderer.writeSolution(dir, soln, maxNodes)
    }
    opt
  }
}

object CountdownConfig {

  def apply(config: Config): CountdownConfig = {
    import args4c.implicits._
    val targetNumber = config.getInt("target")
    val inputNumbers = config.asList("from").toSet.map { s: String =>
      s.toInt
    }
    val maxPop = config.getInt("maxPopulationSize")
    val minEquationSize = config.getInt("minEquationSize")
    val maxEquationSize = config.getInt("maxEquationSize") match {
      case n if n <= 0 => inputNumbers.size
      case n           => n
    }
    val maxGenerations = config.getInt("maxGenerations")
    val mutationProbability = config.getDouble("mutationProbability") //0.01
    val writeSolution = config.getString("output.dir") match {
      case "" => Option.empty[(Path, Int)]
      case dir =>
        val max = config.getInt("output.maxNodes")
        Option(Paths.get(dir) -> max)
    }

    val algoSettings: AlgoSettings[Equation] = {
      implicit val ordering = Equation.orderingForTarget(targetNumber)
      implicit val show = Equation.showForTarget(targetNumber)
      val builder =
        AlgoSettings[Equation](maxPopulationSize = maxPop,
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
            val mutate = Seed.nextInt(equation.size - 1).flatMap { index =>
              equation.mutateAt(index, inputNumbers)
            }
            mutate.run(rnd).value
        }
    }

    val seed = config.getString("seed") match {
      case ""   => Seed()
      case seed => Seed(seed.toLong)
    }

    val log = if (config.getBoolean("debug")) { (g: Generation[Equation]) =>
      {
        val (gen, population) = g
        println(population.mkString(s"Generation $gen:\n", "\n", "\n\n"))
      }
    } else { (_: Generation[Equation]) =>
      {}
    }
    new CountdownConfig(
      settings = algoSettings,
      rand = seed,
      inputValues = inputNumbers,
      targetValue = targetNumber,
      debug = log,
      minEquationSize = minEquationSize,
      maxEquationSize = maxEquationSize,
      writeSolution = writeSolution
    )
  }

  def apply(targetNumber: Int,
            inputNumbers: Set[Int],
            seed: Seed = Seed(),
            debug: Boolean = true): CountdownConfig = {
    val config = ConfigFactory.parseString(s"""target=$targetNumber
         |from="${inputNumbers.mkString(",")}"
         |seed=${seed.long}
         |debug=$debug
         |""".stripMargin).withFallback(ConfigFactory.load())
    apply(config)

  }

}
