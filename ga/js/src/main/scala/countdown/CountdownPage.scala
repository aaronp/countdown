package countdown

import ga.GeneticAlgo.Generation
import ga.{HtmlRenderer, Node}
import org.scalajs.dom.html.Div
import org.scalajs.dom.{document, window}
import scalatags.JsDom.all._

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

// thanks 'https://www.sanwebe.com/2014/08/css-html-forms-designs'
@JSExportTopLevel("CountdownPage")
object CountdownPage {

  def divById(id: String): Div = elmById(id) match {
    case div: Div => div
  }

  def elmById(id: String) = document.getElementById(id)

  def onSolve(resultsId: String, resultCanvasId: String)(cfg: CountdownConfig,
                                                         maxNodes: Int) = {
    window.console.info(
      s"$cfg to be rendered to $resultCanvasId using $maxNodes")
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

        window.console.log(s"Setting '$resultsId' to $scriptContents")
        divById(resultsId).innerHTML = ""

        divById(resultsId).appendChild(script(scriptContents).render)
      case None =>
        window.console.log("No solution")
        divById(resultsId).innerHTML = "No Solution"
    }
  }

  @JSExport
  def render(configId: String, resultsId: String, resultCanvasId: String) = {

    def logGen(generation: Generation[Equation]) = {
      val (gen, population) = generation
      window.console.info(s"Generation $gen:\n")
      population.foreach { p =>
        window.console.info(p.toString)
      }
      window.console.info("-" * 80)
    }

    val form = ConfigForm(logGen, onSolve(resultsId, resultCanvasId))

    val container = divById(configId)
    container.innerHTML = ""
    container.appendChild(form.render)
  }
}
