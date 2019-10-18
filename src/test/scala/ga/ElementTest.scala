package ga

class ElementTest extends BaseSpec {

  "Element.eval" should {
    "evaluate 12 / 3 + 6 / 1 + 19" in {
      val eq = Element.parse("12 / 3 + 6 / 1 + 19")
      Element.eval(eq) shouldBe Some(29)
    }
  }
  "Element.populate" should {
    "produce a populate of equations" in {
      val (_, population) = Element.populate(Set(1,12,19,7,14,6,3), 5, 8, 10, Seed(1234))
      val expectedStr = """14 / 12 * 7 - 6 - 1
          |1 * 14 + 6 * 12 / 7 * 19
          |14 * 1 / 12 * 19 / 6
          |19 * 3 * 7 * 12 / 6 * 1 - 14
          |7 / 3 - 14 / 12 * 19 * 1 + 6
          |1 - 6 - 14 * 3 / 7 * 19
          |1 * 12 + 7 / 3 / 6 - 19 + 14
          |1 * 7 * 14 - 6 + 3 * 12
          |3 * 19 / 14 / 12 + 7 / 1 + 6
          |12 + 7 - 1 / 3 / 14 - 6
          |14 * 7 + 12 / 19 / 3 - 1 * 6""".stripMargin

      population should contain theSameElementsInOrderAs (expectedStr.linesIterator.map(Element.parse).toList)

    }
  }
}
