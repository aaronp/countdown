package countdown

import java.nio.file.{Path, Paths}

import com.typesafe.config.{Config, ConfigFactory}
import ga.GeneticAlgo.Generation
import ga.{AlgoSettings, Seed}

object AsCountdownConfig {

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

    val seed = config.getString("seed") match {
      case ""   => Seed()
      case seed => Seed(seed.toLong)
    }

    val algoSettings: AlgoSettings[Equation] = CountdownConfig.makeAlgoSettings(
      targetNumber,
      inputNumbers,
      maxPop,
      mutationProbability,
      maxGenerations
    )

    def silent(g: Generation[Equation]) = {}

    def printGen(g: Generation[Equation]) = {
      val (gen, population) = g
      println(population.mkString(s"Generation $gen:\n", "\n", "\n\n"))
    }

    val log = if (config.getBoolean("debug")) {
      printGen _
    } else {
      silent _
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
}
