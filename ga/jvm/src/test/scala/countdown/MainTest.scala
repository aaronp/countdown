package countdown

import ga.BaseSpec
import eie.io._

class MainTest extends BaseSpec {
  "Main.run" should {
    "write an html solution when output.dir is set" in {
      val dir = s"target/temp-${System.currentTimeMillis}".asPath.mkDirs()
      try {
        val solnOpt = Main.runMain(Array(s"output.dir=${dir.toAbsolutePath}")).flatten
        solnOpt.isDefined shouldBe true
      } finally {
        dir.delete()
      }
    }
  }

}
