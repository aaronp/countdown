package countdown

import ga.GeneticAlgo.Generation
import ga.{HtmlRenderer, Node}
import org.scalajs.dom.html.Div
import org.scalajs.dom.{document, window}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("CountdownPage")
object CountdownPage {

  def divById(id: String): Div = elmById(id) match {
    case div: Div => div
  }

  def elmById(id: String) = document.getElementById(id)

  // thanks 'https://www.sanwebe.com/2014/08/css-html-forms-designs'
  def onSolve(resultsId: String, resultCanvasId: String)(cfg: CountdownConfig,
                                                         maxNodes: Int) = {
    cfg.solve() match {
      case Some(soln) =>
        window.console.log(s"Solved: $soln")
        val solutionNode: Node = {
          implicit val show = Equation.showForTarget(cfg.targetValue)

          val n = Node(soln, maxNodes)
          n.copy(color = "#FF0000")
        }

        val scriptContents =
          HtmlRenderer.javascriptCode(solutionNode, resultCanvasId)

//        divById(resultsId).innerHTML = s"<script>${scriptContents}</script>"
        divById(resultsId).innerHTML = scriptContents
      case None =>
        window.console.log("No solution")
        divById(resultsId).innerHTML = "No Solution"
    }
  }

  @JSExport
  def render(configId: String, resultsId: String, resultCanvasId: String) = {

    def logGen(eq: Generation[Equation]) = {
      window.console.info(s"$eq")
    }

    val form = ConfigForm(logGen, onSolve(resultsId, resultCanvasId))

    val container = divById(configId)
    container.innerHTML = ""
    container.appendChild(form.render)
  }
}
