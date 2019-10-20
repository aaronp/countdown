package countdown

import args4c.ConfigApp
import com.typesafe.config.Config
import ga.Geneology

object Main extends ConfigApp {
  override type Result = Option[Geneology[Equation]]

  override def run(config: Config): Option[Geneology[Equation]] = {
    val countdownConfig = AsCountdownConfig(config)
    val opt = countdownConfig.solve()
    opt match {
      case Some(solution) =>
        println(solution)

        for {
          (dir, maxNodes) <- countdownConfig.writeSolution
          soln <- opt
        } {
          implicit val show =
            Equation.showForTarget(countdownConfig.targetValue)
          SolutionHtml.writeSolution(dir, soln, maxNodes)
        }

      case None =>
        println("No solution found")
    }
    opt
  }
}
