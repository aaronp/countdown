package countdown

import java.nio.file.Paths

import ga.{BaseSpec, Offspring, Seed}

class AsCountdownConfigTest extends BaseSpec {

  "CountdownConfig" should {
    "solve equations" in {
      val targetNumber = 15
      val cfg = AsCountdownConfig(targetNumber,
                                  Set(12, 19, 7, 14, 6, 3),
                                  Seed(1234).next,
                                  debug = true)

      val Some(soln @ Offspring(result, gen, id, mom, dad)) = cfg.solve

      assertions(soln)

      gen shouldBe 3
      id shouldBe 29

      result shouldBe Equation.parse("19 - 12 / 3")
      mom.value shouldBe Equation.parse("19-12/3-12")
      dad.value shouldBe Equation.parse("3*19+7")
    }

    "solve random equations" in {
      val targetNumber = 7

      def config(seed: Seed) =
        AsCountdownConfig(targetNumber, Set(3, 5, 6, 8), seed, debug = false)
          .copy(writeSolution = Option(Paths.get("target/report") -> 20))

      locally {
        val seed = Seed(17)
        val Some(soln @ Offspring(result, gen, id, mom, dad)) =
          config(seed).solve

        assertions(soln)

        gen shouldBe 3
        id shouldBe 51
        result shouldBe Equation.parse("8 - 6 + 5")
        mom.value shouldBe Equation.parse("8 - 6 + 5 - 3")
        dad.value shouldBe Equation.parse("8 * 5 - 8")

      }

      withClue(
        "Using a different random seed comes up with a different solution") {
        val seed = Seed(21)
        val Some(soln @ Offspring(result, gen, id, mom, dad)) =
          config(seed).solve

        assertions(soln)

        gen shouldBe 4
        id shouldBe 34

        result shouldBe Equation.parse("5 * 3 - 8")
        mom.value shouldBe Equation.parse("5 * 3 + 6 / 3")
        dad.value shouldBe Equation.parse("8 * 3 - 8")
      }
    }
  }

  /**
    * It's helpful as I refactor/play with the code to manually verify/validate the results,
    * then use this to print out some assertions I can bake in to ensure I don't regress.
    *
    * @param soln
    */
  private def assertions(soln: Offspring[Equation]) = {
    val Offspring(result, gen, id, mom, dad) = soln
    //    println(
    (s"""
        |gen shouldBe $gen
        |id shouldBe $id
        |result shouldBe Equation.parse("${result.expressionString}")
        |mom.value shouldBe Equation.parse("${mom.value.expressionString}")
        |dad.value shouldBe Equation.parse("${dad.value.expressionString}")
        |""".stripMargin)
  }

}
