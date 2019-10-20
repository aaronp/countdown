package countdown

import java.nio.file.{Files, Path, Paths}

import cats.Show
import ga.HtmlRenderer.javascriptCode
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
    val javascript = javascriptCode(solutionNode)

    Files.createDirectories(dir)
    writeJsLib(dir, "springy.js")
    writeJsLib(dir, "springyui.js")
    writeJsLib(dir, "jquery.min.js")

    Files.write(dir.resolve("solution.html"), javascript.getBytes)
  }

  def writeTo[A: Show](geneology: Geneology[A], maxNodes: Int) = {
    val javascript = javascriptCode(Node(geneology, maxNodes))
    Files.write(Paths.get("solution.html"), javascript.getBytes)
  }
}
