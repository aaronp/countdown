package countdown

class ElementTest extends BaseSpec {

//  "Element.populate" should {
//    "populate random inputs" in {
//      //Random.nextInt(values.size)
//      def nextInt(size: Int) = (size - 1).max(0)
//
//      Element.populate(Set(1, 2, 3, 4, 5), nextInt) shouldBe Seq(4, 5, 1, 2, 3)
//    }
//  }
  "Element.populate" should {
    "produce a populate of equations" in {
      //Random.nextInt(values.size)


      val (_, population) = Element.populate(Set(1,12,19,7,14,6,3), 5, 8, 20, Seed(1234))
      println(population.map(_.mkString(" ")).mkString("\n"))

    }
  }
}
