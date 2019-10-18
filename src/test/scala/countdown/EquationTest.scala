package countdown

import ga.{BaseSpec, Seed}

class EquationTest extends BaseSpec {

  "Equation.mutateAt" should {
    "alter the equation" in {
      val eq = Equation.parse("10 / 12 + 32 - 400 + 600 + 7 / 2")

      println(eq.mutateAt(0, Set(1000)).runA(Seed(1234)).value)
      println(eq.mutateAt(0, Set(1000, 2000)).runA(Seed(1234)).value)
      println(eq.mutateAt(0, Set(1000, 3000, 2000)).runA(Seed(1234)).value)

      println(eq.mutateAt(1, Set(1000, 3000, 2000)).runA(Seed(1234)).value)
      println(eq.mutateAt(2, Set(1000, 3000, 2000)).runA(Seed(1234)).value)
      println(eq.mutateAt(3, Set(1000, 3000, 2000)).runA(Seed(1234)).value)
    }
  }
  "Equation.combineAt" should {
    val first = Equation.parse("1 + 2 - 3 * 4")
    val second = Equation.parse("10 / 12 + 32 - 400 + 600 + 7 / 2")
    "swap at an index" in {
      def at(i: Int) = {
        val res = first.combineAt(second, i)

        s"""$first
           |$second
           |--------------------------
           |$res
           |
           |""".stripMargin
      }

      println((0 to 10).map(at).mkString("\n"))
    }
  }
}
