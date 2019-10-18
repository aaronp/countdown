package ga

class SeedTest extends BaseSpec {

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
