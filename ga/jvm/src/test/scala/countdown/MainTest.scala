package countdown

import ga.BaseSpec
import eie.io._

class MainTest extends BaseSpec {
  "Main.run" should {
    "write an html solution when output.dir is set" in {
      val dir = s"target/temp-${System.currentTimeMillis}".asPath.mkDirs()
      try {
        val Some(soln) = Main.runMain(Array(s"output.dir=${dir.toAbsolutePath}", "target=12", "from=3,4,5", "seed=1234")).flatten
        dir.children.map(_.fileName) should contain ("solution.html")
        soln.value.expressionString shouldBe "3 + 5 + 4"
      } finally {
        dir.delete()
      }
    }
  }
}
