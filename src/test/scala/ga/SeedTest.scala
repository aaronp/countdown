package ga

class SeedTest extends BaseSpec {

  "Seed.nextDouble" should {

    Seq(
      (Seed(-4611776864058626593L), 0.500009849503075),
      (Seed(-920016391455073L), 9.974837703378644E-5),
      (Seed(-9223370912754974934L), 0.9999998781248554)
    ).foreach {
      case (seed, expected) =>
        s"return $expected for seed ${seed.long}" in {
          Seed.nextDouble.run(seed).value._2 shouldBe expected
        }
    }

    val ranges = Seq(
      (0.0, 0.0001),
      (0.4999, 0.50001),
      (0.999, 1.0)
    )
    ranges.foreach {
      case (from, to) =>

        s"return values between $from and $to for some value" in {
          val (_, found) = Iterator.from(0).take(10000).foldLeft((Seed(0), Seq.empty[(Seed, Double)])) {
            case ((s, found), _) =>
              val (next, value) = Seed.nextDouble.run(s).value
              val list = if (value >= from && value <= to) {
                (s, value) +: found
              } else {
                found
              }
              (next, list)
          }
          found should not be empty
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
        randoms.find(x => x > max || x < 0) shouldBe None
      }
    }
  }
}
