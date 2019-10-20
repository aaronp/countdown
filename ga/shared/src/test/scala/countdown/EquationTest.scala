package countdown

import ga.{BaseSpec, Seed}

class EquationTest extends BaseSpec {

  "Equation.eval" should {
    "evaluate 12 / 3 + 6 / 1 + 19" in {
      val eq = Equation.parse("12 / 3 + 6 / 1 + 19")
      eq.eval shouldBe Some(29)
    }
  }
  "Equation.evalReduced" should {
    "Evaluate equations" in {
      val eq = Equation.parse("14 - 1 - 7 - 6 - 1 - 7 - 7")
      Equation.evalReduced(eq.expression) shouldBe -15
      Equation.parse("14 - 1 - 7 - 6 - 1 - 7 - 7").diff(15).get shouldBe 30
    }
  }
  "Equation.reduce" should {
    "return a none for numbers which can't be evenly divided" in {
      Equation.reduce(Equation.parse("14 / 7 + 2").expression) shouldBe Some(
        Seq(Num(2), Add, Num(2)))
      Equation.reduce(Equation.parse("14 / 8 - 9").expression) shouldBe None
      Equation.reduce(Equation.parse("12 * 6 / 2").expression) shouldBe Some(
        Seq(Num(36)))
      Equation.reduce(Equation.parse("12 * 6 / 2 * 1").expression) shouldBe Some(
        Seq(Num(36)))
      Equation.reduce(Equation.parse("98 + 12 * 6 + 1").expression) shouldBe Some(
        Seq(Num(98), Add, Num(72), Add, Num(1)))
    }
  }
  "Equation.truncate" should {
    "reduce the equation length but ensure we don't end in an operation" in {
      val eq = Equation.parse("10 / 12 + 32 - 400 + 600 + 7 / 2")
      eq.truncate(0) shouldBe Equation.parse("10")
      eq.truncate(1) shouldBe Equation.parse("10")
      eq.truncate(2) shouldBe Equation.parse("10")
      eq.truncate(3) shouldBe Equation.parse("10 / 12")
      eq.truncate(4) shouldBe Equation.parse("10 / 12")
      eq.truncate(eq.size) shouldBe eq
      eq.truncate(eq.size + 1) shouldBe eq
    }
  }
  "Equation.mutateAt" should {
    "alter the equation" in {
      val eq = Equation.parse("10 / 12 + 32 - 400 + 600 + 7 / 2")

      eq.mutateAt(0, Set(1000, 3000, 2000))
        .runA(Seed(1234))
        .value shouldBe Equation.parse("3000 / 12 + 32 - 400 + 600 + 7 / 2")
      eq.mutateAt(3, Set(1000, 3000, 2000))
        .runA(Seed(1234))
        .value shouldBe Equation.parse("10 / 12 - 32 - 400 + 600 + 7 / 2")
    }
  }
  "Equation.combineAt" should {
    val first = Equation.parse("1 + 2 - 3 * 4")
    val second = Equation.parse("10 / 12 + 32 - 400 + 600 + 7 / 2")
    "swap at an index" in {
      first
        .combineAt(second, 0)
        .expressionString shouldBe "1 / 12 + 32 - 400 + 600 + 7 / 2"
      first
        .combineAt(second, 1)
        .expressionString shouldBe "1 + 12 + 32 - 400 + 600 + 7 / 2"
      first
        .combineAt(second, 2)
        .expressionString shouldBe "1 + 2 + 32 - 400 + 600 + 7 / 2"
      first
        .combineAt(second, 3)
        .expressionString shouldBe "1 + 2 - 32 - 400 + 600 + 7 / 2"
      first
        .combineAt(second, 4)
        .expressionString shouldBe "1 + 2 - 3 - 400 + 600 + 7 / 2"
      first
        .combineAt(second, 5)
        .expressionString shouldBe "1 + 2 - 3 * 400 + 600 + 7 / 2"
      first
        .combineAt(second, 6)
        .expressionString shouldBe "1 + 2 - 3 * 4 + 600 + 7 / 2"
      first
        .combineAt(second, 7)
        .expressionString shouldBe "1 + 2 - 3 * 4 + 600 + 7 / 2"
    }
  }

  "Equation.populate" should {
    "produce a populate of equations" in {
      val (_, population) =
        Equation.populate(Set(1, 12, 19, 7, 14, 6, 3), 5, 8, 10, Seed(1234))
      val expectedStr =
        """14 * 12 + 7 * 6 + 1
          |1 / 14 / 6 / 12 / 7 - 19
          |14 * 1 + 12 * 19 + 6
          |19 - 3 / 7 - 12 - 6 / 1 / 14
          |7 + 3 * 14 + 12 * 19 + 1 * 6
          |1 - 6 - 14 / 3 - 7 / 19
          |1 + 12 * 7 + 3 * 6 + 19 * 14
          |1 / 7 - 14 - 6 - 3 - 12
          |3 + 19 * 14 + 12 * 7 + 1 * 6
          |12 / 7 - 1 / 3 / 14 - 6
          |14 + 7 * 12 + 19 * 3 + 1 * 6""".stripMargin

      population should contain theSameElementsInOrderAs (expectedStr.linesIterator
        .map(Equation.parse)
        .toList)

    }
  }
}
