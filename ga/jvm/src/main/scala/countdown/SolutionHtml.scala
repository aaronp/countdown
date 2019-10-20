package countdown

import java.nio.file.{Files, Path}

import cats.Show
import ga.HtmlRenderer.htmlCode
import ga.{Geneology, Node}

object SolutionHtml {

  private def writeJsLib(dir: Path, resource: String) = {
    val path = dir.resolve(resource)
    if (!Files.exists(path)) {
      val is = getClass.getClassLoader.getResource(resource).openStream()
      Files.copy(is, path)
    }
  }

  def writeSolution[A: Show](dir: Path, soln: Geneology[A], maxNodes: Int) = {
    val solutionNode = {
      val n = Node(soln, maxNodes)
      n.copy(color = "#FF0000")
    }
    val javascript = htmlCode(solutionNode)

    Files.createDirectories(dir)
    writeJsLib(dir, "springy.js")
    writeJsLib(dir, "springyui.js")
    writeJsLib(dir, "jquery.min.js")

    val solnHtml = dir.resolve("solution.html")
    Files.write(solnHtml, javascript.getBytes)
    println(s"Wrote ${solnHtml.toAbsolutePath}")
  }

}
