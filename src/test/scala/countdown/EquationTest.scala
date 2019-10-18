package countdown

import ga.{BaseSpec, Seed}

class EquationTest extends BaseSpec {

  "Equation.evalReduced" should {
    "Evaluate equations" in {
      val eq = Equation.parse("14 - 1 - 7 - 6 - 1 - 7 - 7")
      Equation.evalReduced(eq.expression) shouldBe -15
      Equation.parse("14 - 1 - 7 - 6 - 1 - 7 - 7").diff(15) shouldBe 30
    }
  }
  "Equation.reduce" should {
    "return a none for numbers which can't be evenly divided" in {
      Equation.reduce(Equation.parse("14 / 7 + 2").expression) shouldBe Some(Seq(Num(2), Add, Num(2)))
      Equation.reduce(Equation.parse("14 / 8 - 9").expression) shouldBe None
      Equation.reduce(Equation.parse("12 * 6 / 2").expression) shouldBe Some(Seq(Num(36)))
      Equation.reduce(Equation.parse("12 * 6 / 2 * 1").expression) shouldBe Some(Seq(Num(36)))
      Equation.reduce(Equation.parse("98 + 12 * 6 + 1").expression) shouldBe Some(Seq(Num(98), Add, Num(72), Add, Num(1)))
    }
  }
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
