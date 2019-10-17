package countdown

import cats.data.State
import org.scalatest.FunSuite

import scala.util.Random

class SeedTest extends BaseSpec {

  "Seed.tuples" should {
    "return different random values" in {

      Seed()

      val badState: State[Random, Int] = State.apply[Random, Int] { r =>
        r -> r.nextInt()
      }
      val (r, x) = badState.run(new Random).value
      val (r2, x2) = badState.run(new Random).value
      println(x)
      println(x2)
    }
  }
}
