package countdown

import countdown.Solver.showForTarget
import ga.{BaseSpec, Geneology, Node, Seed}

class SolverTest extends BaseSpec {

  "Solver" should {
    "solve equations" in {
      val targetNumber = 697
      val result: Option[Geneology[Equation]] = Solver.solve(targetNumber, Set(4,12,6,7, 9, 21, 17, 4, 2), Seed(123).next)
      println(result)

      implicit val show = showForTarget(targetNumber)
      result.foreach { soln: Geneology[Equation] =>

        Node.writeSolution(soln)
      }
    }
  }
}
