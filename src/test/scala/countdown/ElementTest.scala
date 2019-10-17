package countdown

class ElementTest extends BaseSpec {

  "Element.populate" should {
    "populate random inputs" in {
      //Random.nextInt(values.size)
      def nextInt(size: Int) = (size - 1).max(0)
      Element.populate(Set(1, 2, 3, 4, 5), nextInt) shouldBe Seq(4, 5, 1, 2, 3)
    }
  }
}
