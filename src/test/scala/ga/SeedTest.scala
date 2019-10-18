package ga

class SeedTest extends BaseSpec {

  "Seed.nextDouble" should {

    val seedAndDouble = Seq(
      (6997317046046405250L, 0.500009849503075),
      (3092481303161922626L, 9.974837703378644E-5),
      (6129515119770464689L, 0.9999998781248554)
    )
    seedAndDouble.foreach {
      case (seed, expected) =>

        s"return $expected for seed $seed" in {
          val (_, found) = Iterator.from(0).take(10000000).foldLeft((Seed(0), Seq.empty[(Seed, Double)])) {
            case ((s, found), _) =>
              val (next, value) = Seed.nextDouble.run(s).value
              val list = if (value >= from && value <= to) {
                (next, value) +: found
              } else {
                found
              }
              (next, list)
          }
          val midpoint = from + (to - from / 2)
          val closest = found.minBy{
            case (_, value) => (value - midpoint).abs
          }
          println(closest)
        }
    }
    "return values between 0.0 and 1.0" in {
      (0 to 1000).foreach { i =>
        val actual: Double = Seed.nextDouble.run(Seed(i * 53).next).value._2
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
          Seed.nextInt(max).run(Seed(i * 53).next).value._2
        }
        randoms.distinct.sorted should contain theSameElementsInOrderAs (0 to max)
      }
    }
  }
}
