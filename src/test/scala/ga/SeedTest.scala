package ga

class SeedTest extends BaseSpec {

  "Seed.nextDouble" should {
    "return values between 0.0 and 1.0" in {
      (0 to 1000).foreach { i =>
        val actual: Double = Seed.nextDouble.run(Seed(i)).value._2
        actual should be >= 0.0
        actual should be <= 1.0
      }
    }
  }
  "Seed.nextInt" should {
    for {
      max <- (0 to 100)
    } {
      s"return ints between zero and $max, inclusive" in {
        val randoms = (0 to max * 2).map { i =>
          Seed.nextInt(max).run(Seed(i)).value._2
        }
        randoms.distinct.sorted should contain theSameElementsInOrderAs (0 to max)
      }
    }
  }
}
