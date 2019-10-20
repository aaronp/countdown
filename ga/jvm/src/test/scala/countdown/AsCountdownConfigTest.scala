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

      val Some(soln@Offspring(result, gen, id, mom, dad)) = cfg.solve

      //      assertions(soln)

      gen shouldBe 4
      id shouldBe 195
      result shouldBe Equation.parse("3 * 7 - 6")
      mom.value shouldBe Equation.parse("3 * 7 - 7 / 14 - 3 - 12")
      dad.value shouldBe Equation.parse("7 * 7 / 6")
    }

    "solve random equations" in {
      val targetNumber = 7

      def config(seed: Seed) =
        AsCountdownConfig(targetNumber, Set(3, 5, 6, 8), seed, debug = false)
          .copy(writeSolution = Option(Paths.get("target/report") -> 20))

      locally {
        val seed = Seed(17)
        val Some(soln@Offspring(result, gen, id, mom, dad)) =
          config(seed).solve

        //        assertions(soln)

        gen shouldBe 0
        id shouldBe 120
        result shouldBe Equation.parse("8 - 6 + 5")
        mom.value shouldBe Equation.parse("8 - 6 - 5 - 3")
        dad.value shouldBe Equation.parse("8 * 6 + 5")
      }

      withClue(
        "Using a different random seed comes up with a different solution") {
        val seed = Seed(21)
        val Some(soln@Offspring(result, gen, id, mom, dad)) =
          config(seed).solve

        //        assertions(soln)

        gen shouldBe 0
        id shouldBe 190
        result shouldBe Equation.parse("8 - 6 + 5")
        mom.value shouldBe Equation.parse("8 - 6 / 5 / 3")
        dad.value shouldBe Equation.parse("8 * 6 + 5")
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
    println(
      s"""
         |gen shouldBe $gen
         |id shouldBe $id
         |result shouldBe Equation.parse("${result.expressionString}")
         |mom.value shouldBe Equation.parse("${mom.value.expressionString}")
         |dad.value shouldBe Equation.parse("${dad.value.expressionString}")
         |""".stripMargin)
  }

}
