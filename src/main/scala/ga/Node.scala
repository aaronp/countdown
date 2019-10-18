package ga

import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger

import cats.Show

final case class Node(id: String, label: String, parents: Set[Node])

object Node {
  private def writeJsLib(resource: String) = {
    val path = Paths.get(resource)
    if (!Files.exists(path)) {
      val is = getClass.getClassLoader.getResource(resource).openStream()
      Files.copy(is, path)
    }
  }

  def writeSolution[A: Show](soln: Geneology[A]) = {
    val solutionNode = {
      val n = Node(soln)
      n.copy(label = "ANSWER:" + n.label)
    }
    val javascript = Node.javascriptCode(solutionNode)

    writeJsLib("springy.js")
    writeJsLib("springyui.js")

    Files.write(Paths.get("solution.html"), javascript.getBytes)
  }


  import cats.syntax.show._

  def apply[A: Show](geneology: Geneology[A]): Node = {
    forGeneology(geneology, new AtomicInteger(0))
  }

  def writeTo[A: Show](geneology: Geneology[A]) = {
    getClass.getClassLoader.getResource("springy.js")
    getClass.getClassLoader.getResource("springyui.js")
    val javascript = javascriptCode(Node(geneology))
    Files.write(Paths.get("solution.html"), javascript.getBytes)
  }

  def javascriptCode(node: Node): String = {
    val defns = nodeCode(node)
    val edges = edgeCode(node)

    s"""<html>
       |<body>
       |<script src="http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"></script>
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
    val varDefn = s"""var ${node.id} = graph.newNode({label: '${node.label}$closing""".stripMargin
    node.parents.map(nodeCode).mkString(s"$varDefn\n", "\n", "\n")
  }

  private def edgeCode(node: Node): String = {
    val parents = node.parents.toSeq
    val edges = parents.map { p =>
      val code = s"""graph.newEdge(${node.id}, ${p.id}, {color: '#00FF00'});""".stripMargin

      code
    }

    val parentEdges = parents.map(edgeCode)
    (edges ++ parentEdges).mkString("\n")
  }

  private def forGeneology[A: Show](geneology: Geneology[A], id: AtomicInteger): Node = {
    geneology match {
      case Origin(value) => Node(s"node${id.incrementAndGet()}", s"Original value: ${value.show}", Set.empty)
      case Mutation(value, from) =>
        val fromNode = forGeneology(from, id)
        Node(s"node${id.incrementAndGet()}", s"Mutation ${value.show}", Set(fromNode))
      case Offspring(value, gen, nr, mom, dad) =>
        val momNode = forGeneology(mom, id)
        val dadNode = forGeneology(dad, id)
        Node(s"node${id.incrementAndGet()}", s"${value.show}", Set(momNode, dadNode))
    }
  }
}
