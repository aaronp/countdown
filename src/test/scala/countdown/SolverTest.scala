package countdown

import countdown.Solver.showForTarget
import ga.{BaseSpec, Geneology, Node, Seed}

class SolverTest extends BaseSpec {

  "Solver" should {
    "solve equations" in {
      val targetNumber = 15
      val result: Option[Geneology[Equation]] = Solver.solve(targetNumber, Set(1, 12, 19, 7, 14, 6, 3), Seed().next)
      println(result)

      implicit val show = showForTarget(targetNumber)
      result.foreach { soln: Geneology[Equation] =>

        Node.writeSolution(soln, 30)
      }
    }
  }
}
