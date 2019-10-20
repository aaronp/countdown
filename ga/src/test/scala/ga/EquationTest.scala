package ga

import countdown.Equation

class EquationTest extends BaseSpec {

  "Equation.eval" should {
    "evaluate 12 / 3 + 6 / 1 + 19" in {
      val eq = Equation.parse("12 / 3 + 6 / 1 + 19")
      eq.eval shouldBe Some(29)
    }
  }
  "Equation.ordering" should {
    "put the closest values to the top" in {
      val (_, population) =
        Equation.populate(Set(2, 7, 19, 21, 50), 10, Seed(100))
      population.foreach(println)
      implicit val ord = Equation.orderingForTarget(19)
      val sorted = population.sorted
      sorted.foreach(println)
      sorted.foreach(println)
//      7 + 2 == 9
//      19 - 2 - 21 == -4
//      50 + 19 == 69
//      19 + 50 == 69
//      50 + 19 == 69
//      19 + 50 == 69
//      19 / 7 / 2 - 21 / 50 == ???
//      7 - 21 / 19 == ???
//      50 / 21 - 7 == ???
//      50 / 2 / 21 == ???

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
