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

  def writeSolution[A: Show](soln: Geneology[A], maxNodes: Int) = {
    val solutionNode = {
      val n = Node(soln, maxNodes)
      n.copy(label = "ANSWER:" + n.label)
    }
    val javascript = Node.javascriptCode(solutionNode)

    writeJsLib("springy.js")
    writeJsLib("springyui.js")

    Files.write(Paths.get("solution.html"), javascript.getBytes)
  }


  import cats.syntax.show._

  def apply[A: Show](geneology: Geneology[A], maxNodes: Int): Node = {
    forGeneology(geneology, new AtomicInteger(maxNodes), "")
  }

  def writeTo[A: Show](geneology: Geneology[A], maxNodes: Int) = {
    getClass.getClassLoader.getResource("springy.js")
    getClass.getClassLoader.getResource("springyui.js")
    val javascript = javascriptCode(Node(geneology, maxNodes))
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

  private def forGeneology[A: Show](geneology: Geneology[A], idCounter: AtomicInteger, prefix:String): Node = {
    val id = idCounter.decrementAndGet()
    val nodeId = s"${prefix}_n$id".replaceAllLiterally("-", "xx")
    geneology match {
      case Origin(value) => Node(nodeId, s"${value.show}", Set.empty)
      case Mutation(value, from) =>
        val fromNode = if (id <= 0) Set.empty[Node] else Set(forGeneology(from, idCounter, prefix))
        Node(nodeId, s"Mutation ${value.show}", fromNode)
      case Offspring(value, gen, nr, mom, dad) =>
        val parents = if (id <= 0) Set.empty[Node] else {
          val half = idCounter.get() / 2

          val momCounter = new AtomicInteger(half)
          val momNode = forGeneology(mom, momCounter, s"m$nodeId")

          val dadCounter = new AtomicInteger(half)
          val dadNode = forGeneology(dad, dadCounter, s"d$nodeId")

          Set(momNode, dadNode)
        }
        Node(nodeId, s"@$gen:$nr ${value.show}", parents)
    }
  }
}
