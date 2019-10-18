package countdown

import ga.BaseSpec

class EquationTest extends BaseSpec {

  "Equation.combineAt" should {
    val first = Equation.parse("1 + 2 - 3 * 4")
    val second = Equation.parse("10 / 12 + 32 - 400 + 600 + 7 / 2")
    "swap at an index" in {
      def at(i : Int) = {
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
