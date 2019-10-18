package ga

class WeightedRandomTest extends BaseSpec {

  "WeightedRandom" should {
    "return the value from the corresponding bucket" in {
      val buckets = Seq("ten percent chance" -> 0.1, "thirty percent" -> 0.3, "sixty percent" -> 0.6)
      WeightedRandom(0.1, buckets) shouldBe "ten percent chance"
      WeightedRandom(0.01, buckets) shouldBe "ten percent chance"
      WeightedRandom(0.11, buckets) shouldBe "thirty percent"
      WeightedRandom(0.3, buckets) shouldBe "thirty percent"
      WeightedRandom(0.39, buckets) shouldBe "thirty percent"
      WeightedRandom(0.4, buckets) shouldBe "thirty percent"
      WeightedRandom(0.41, buckets) shouldBe "sixty percent"
      WeightedRandom(1.0, buckets) shouldBe "sixty percent"
    }
  }
}
