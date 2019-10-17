package countdown

class SolverTest extends BaseSpec {

  "Solver" should {
    "solve equations" in {
      val result = Solver.solve(123, Set(1, 12, 19, 7, 14, 6, 3))
      println(result)
    }
  }
}
