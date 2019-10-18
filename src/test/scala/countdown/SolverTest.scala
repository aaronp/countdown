package countdown

import ga.{BaseSpec, Seed}

class SolverTest extends BaseSpec {

  "Solver" should {
    "solve equations" in {
      val result = Solver.solve(15, Set(1, 12, 19, 7, 14, 6, 3), Seed(123))
      println(result)
    }
  }
}
