package countdown

import ga.{BaseSpec, Geneology, Seed}

class SolverTest extends BaseSpec {

  "Solver" should {
    "solve equations" in {
      val result: Option[Geneology[Equation]] = Solver.solve(15, Set(1, 12, 19, 7, 14, 6, 3), Seed(123).next)
      println(result)
    }
  }
}
