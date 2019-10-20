package ga

import java.nio.file.{Files, Path, Paths}

import cats.Show

object HtmlRenderer {
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

  def javascriptCode(node: Node): String = {
    val defns = nodeCode(node)
    val edges = edgeCode(node)

    s"""<html>
       |<body>
       |<script src="jquery.min.js"></script>
       |<script src="springy.js"></script>
       |<script src="springyui.js"></script>
       |<script>
       |var graph = new Springy.Graph();
       |
       |${defns}
       |
       |${edges}
       |
       |jQuery(function(){
       |  var springy = window.springy = jQuery('#layout').springy({
       |    graph: graph,
       |    nodeSelected: function(node){
       |      console.log('Node selected: ' + JSON.stringify(node.data));
       |    }
       |  });
       |});
       |</script>
       |
       |<canvas id="layout" width="1280" height="960" />
       |</body>
       |</html>
       |
       |""".stripMargin
  }

  private def nodeCode(node: Node): String = {
    val closing = "'});"
    val varDefn =
      s"""var ${node.id} = graph.newNode({color: '${node.color}', label: '${node.label}$closing""".stripMargin
    node.parents.map(nodeCode).mkString(s"$varDefn\n", "\n", "\n")
  }

  private def edgeCode(node: Node): String = {
    val parents = node.parents.toSeq
    val edges = parents.map { p =>
      val code =
        s"""graph.newEdge(${node.id}, ${p.id}, {color: '#00FF00'});""".stripMargin

      code
    }

    val parentEdges = parents.map(edgeCode)
    (edges ++ parentEdges).mkString("\n")
  }
}
