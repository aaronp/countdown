package ga

class AlgoSettingsTest extends BaseSpec {

  "AlgoSettings.nextMateIndex" should {
    "return a neighbor index roughly 40% of the time, +/-2 roughly 10% of the time, +/-3 roughly 10% of the time, and some other value in range 40% of the time" in {
      val (_, samples) = Iterator.from(0).take(10000).foldLeft(Seed(0) -> Seq.empty[Int]) {
        case ((rnd, found), _) =>
          val (newRnd, x) = AlgoSettings.nextMateIndex(15, 100).run(rnd).value
          newRnd -> (x +: found)
      }
      val size = samples.size.toDouble

      def nAway(n: Int)(x: Int) = x == 15 - n || x == 15 + n

      val oneAway = nAway(1) _
      val twoAway = nAway(2) _
      val threeAway = nAway(3) _

      def theRest(x: Int) = x < 12 || x > 18

      samples.count(oneAway) / size shouldBe 0.4 +- 0.01
      samples.count(twoAway) / size shouldBe 0.1 +- 0.01
      samples.count(threeAway) / size shouldBe 0.1 +- 0.01
      samples.count(theRest) / size shouldBe 0.4 +- 0.01
      samples.foreach { x =>
        x should not be >=(100)
        x should not be <=(0)
        x should not be (15)
      }
    }
    "return a valid index to pair with" in {

      Iterator.from(0).take(1000).foldLeft(Seed(0)) {
        case (s, _) =>

          locally {
            val index = AlgoSettings.nextMateIndex(0, 10).runA(s).value
            index should be > 0
            index should be < 10
          }

          locally {
            val index = AlgoSettings.nextMateIndex(9, 10).runA(s).value
            index should be >= 0
            index should be < 9
          }

          locally {
            val index = AlgoSettings.nextMateIndex(5, 10).runA(s).value
            index should be >= 0
            index should be < 10
            index should not be 5
          }

          AlgoSettings.nextMateIndex(0, 10).runS(s).value
      }

    }
  }

}
