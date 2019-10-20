package countdown

import java.nio.file.Paths

import ga.{BaseSpec, Offspring, Seed}

class CountdownConfigTest extends BaseSpec {

  def assertions(soln: Offspring[Equation]) = {
    val Offspring(result, gen, id, mom, dad) = soln
    println(s"""
         |gen shouldBe $gen
         |id shouldBe $id
         |result shouldBe Equation.parse("${result.expressionString}")
         |mom.value shouldBe Equation.parse("${mom.value.expressionString}")
         |dad.value shouldBe Equation.parse("${dad.value.expressionString}")
         |""".stripMargin)
  }

  "CountdownConfig" should {
    "solve equations" in {
      val targetNumber = 15
      val cfg = CountdownConfig(targetNumber,
                                Set(12, 19, 7, 14, 6, 3),
                                Seed(1234).next,
                                debug = true)

      val Some(Offspring(result, gen, id, mom, dad)) = cfg.solve
      gen shouldBe 2
      id shouldBe 172
      result shouldBe Equation.parse("3 - 7 + 19")
      mom.value shouldBe Equation.parse("3 - 7 / 12 / 14 / 7 / 12")
      dad.value shouldBe Equation.parse("7 / 3 + 19")
    }

    "solve random equations" in {
      val targetNumber = 7

      def config(seed: Seed) =
        CountdownConfig(targetNumber, Set(3, 5, 6, 8), seed, debug = false)
          .copy(writeSolution = Option(Paths.get("target/report") -> 20))

      locally {
        val seed = Seed(17)
        val Some(Offspring(result, gen, id, mom, dad)) = config(seed).solve

        gen shouldBe 0
        id shouldBe 120
        result shouldBe Equation.parse("8 - 6 + 5")
        mom.value shouldBe Equation.parse("8 - 6 - 5 - 3")
        dad.value shouldBe Equation.parse("8 * 6 + 5")
      }

      withClue(
        "Using a different random seed comes up with a different solution") {
        val seed = Seed(18)
        val Some(Offspring(result, gen, id, mom, dad)) = config(seed).solve

        gen shouldBe 22
        id shouldBe 2
        result shouldBe Equation.parse("5 + 6 / 3")
        mom.value shouldBe Equation.parse("5 + 6 / 3")
        dad.value shouldBe Equation.parse("6 + 3 / 3")
      }
    }
  }
}
