package countdown

class ElementTest extends BaseSpec {

  "Element.eval" should {
    "evaluate 12 / 3 + 6 / 1 + 19" in {
      val eq = Element.parse("12 / 3 + 6 / 1 + 19")
      Element.eval(eq) shouldBe Some(29)
    }
  }
  "Element.populate" should {
    "produce a populate of equations" in {
      //Random.nextInt(values.size)


      val (_, population) = Element.populate(Set(1,12,19,7,14,6,3), 5, 8, 20, Seed(1234))
      println(population.map(_.mkString(" ")).mkString("\n"))


    }
  }
}
