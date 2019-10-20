package countdown

import args4c.ConfigApp
import com.typesafe.config.Config
import ga.{Geneology, GeneticAlgo}

object Main extends ConfigApp {
  override type Result = Option[Geneology[Equation]]

  override def run(config: Config): Option[Geneology[Equation]] = {
    val opt = CountdownConfig(config).solve()
    opt match {
      case Some(solution) =>
        println(solution)
      case None =>
        println("No solution found")
    }
    opt
  }
}
