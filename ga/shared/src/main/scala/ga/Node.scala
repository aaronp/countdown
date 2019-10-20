package ga

import java.nio.file.{Files, Paths}
import java.util.concurrent.atomic.AtomicInteger

import cats.Show

final case class Node(id: String,
                      label: String,
                      color: String,
                      parents: Set[Node])

object Node {

  import cats.syntax.show._

  def apply[A: Show](geneology: Geneology[A], maxNodes: Int): Node = {
    forGeneology(geneology, new AtomicInteger(maxNodes), "")
  }

  private def forGeneology[A: Show](geneology: Geneology[A],
                                    idCounter: AtomicInteger,
                                    prefix: String): Node = {
    val id = idCounter.decrementAndGet()
    val nodeId = s"${prefix}_n$id".replaceAllLiterally("-", "xx")
    geneology match {
      case Origin(value) => Node(nodeId, s"${value.show}", "#FF00FF", Set.empty)
      case Mutation(value, from) =>
        val fromNode =
          if (id <= 0) Set.empty[Node]
          else Set(forGeneology(from, idCounter, prefix))
        Node(nodeId, s"Mutation ${value.show}", "#00FF00", fromNode)
      case Offspring(value, gen, nr, mom, dad) =>
        val parents =
          if (id <= 0) Set.empty[Node]
          else {
            val half = idCounter.get() / 2

            val momCounter = new AtomicInteger(half)
            val momNode = forGeneology(mom, momCounter, s"m$nodeId")

            val dadCounter = new AtomicInteger(half)
            val dadNode = forGeneology(dad, dadCounter, s"d$nodeId")

            Set(momNode, dadNode)
          }
        Node(nodeId, s"(Gen$gen#$nr) ${value.show}", "#0000FF", parents)
    }
  }
}
