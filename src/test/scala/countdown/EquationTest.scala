package countdown

import ga.{BaseSpec, Seed}

class EquationTest extends BaseSpec {

  "Equation.mutateAt" should {
    "alter the equation" in {
      val eq = Equation.parse("10 / 12 + 32 - 400 + 600 + 7 / 2")

      eq.mutateAt(0, Set(1000, 3000, 2000)).runA(Seed(1234)).value shouldBe Equation.parse("3000 / 12 + 32 - 400 + 600 + 7 / 2")
      eq.mutateAt(3, Set(1000, 3000, 2000)).runA(Seed(1234)).value shouldBe Equation.parse("10 / 12 - 32 - 400 + 600 + 7 / 2")
    }
  }
  "Equation.combineAt" should {
    val first = Equation.parse("1 + 2 - 3 * 4")
    val second = Equation.parse("10 / 12 + 32 - 400 + 600 + 7 / 2")
    "swap at an index" in {
      first.combineAt(second, 0).expressionString shouldBe "1 / 12 + 32 - 400 + 600 + 7 / 2"
      first.combineAt(second, 1).expressionString shouldBe "1 / 12 + 32 - 400 + 600 + 7 / 2"
      first.combineAt(second, 2).expressionString shouldBe "1 + 12 + 32 - 400 + 600 + 7 / 2"
      first.combineAt(second, 3).expressionString shouldBe "1 + 2 + 32 - 400 + 600 + 7 / 2"
      first.combineAt(second, 4).expressionString shouldBe "1 + 2 - 32 - 400 + 600 + 7 / 2"
      first.combineAt(second, 5).expressionString shouldBe "1 + 2 - 3 - 400 + 600 + 7 / 2"
      first.combineAt(second, 6).expressionString shouldBe "1 + 2 - 3 * 400 + 600 + 7 / 2"
      first.combineAt(second, 7).expressionString shouldBe "1 + 2 - 3 * 400 + 600 + 7 / 2"
    }
  }
}
