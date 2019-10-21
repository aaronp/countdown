package countdown

import ga.{Geneology, HtmlRenderer, Node}
import org.scalajs.dom.html.Div
import org.scalajs.dom.window
import scalatags.JsDom.all._

/**
  * Insert some springy javascript to render the nodes/edges
  */
object ForcedLayoutSolution {

  def renderSolution(scriptContainer: Div,
                     resultCanvasId: String,
                     targetValue: Int,
                     solutionOpt: Option[Geneology[Equation]],
                     maxNodes: Int): Unit = {

    val canvas = HtmlUtils.canvasById(resultCanvasId)

    // let's render the height as less than the full window height
    def windowHeight = (0.8 * window.innerHeight).toInt

    def windowWidth = (0.8 * window.innerWidth).toInt

    canvas.width = windowWidth
    canvas.height = windowHeight
    window.onresize = _ => {
      canvas.width = windowWidth
      canvas.height = windowHeight
    }

    solutionOpt match {
      case Some(soln) =>
        window.console.log(s"Solved: $soln")
        val solutionNode: Node = {
          implicit val show = Equation.showForTarget(targetValue)

          val n = Node(soln, maxNodes)
          n.copy(color = "#FF0000")
        }

        val scriptContents: String =
          HtmlRenderer.javascriptCode(solutionNode, resultCanvasId)

        scriptContainer.innerHTML = ""

        scriptContainer.appendChild(script(scriptContents).render)
      case None =>
        window.console.log("No solution")
        scriptContainer.innerHTML = "No Solution"
    }
  }
}
