package countdown

import ga.GeneticAlgo.Generation
import ga.{Geneology, HtmlRenderer, Node}
import org.scalajs.dom.html.{Canvas, Div}
import org.scalajs.dom.{document, window}
import scalatags.JsDom.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

// thanks 'https://www.sanwebe.com/2014/08/css-html-forms-designs'
@JSExportTopLevel("CountdownPage")
object CountdownPage {

  @JSExport
  def render(configId: String,
             computeContainerId: String,
             scriptContainerId: String,
             resultCanvasId: String) = {

    val computeContainer = divById(computeContainerId)
    computeContainer.innerHTML = ""

    def logGen(generation: Generation[Equation]) = {
      val (gen, population) = generation
      window.console.info(s"Generation $gen:\n")
      population.foreach { gene =>
        val geneString = gene.toString
        window.console.info(geneString)
        computeContainer.appendChild(p(geneString).render)
      }
      window.console.info("-" * 80)
    }

    val scriptContainer = divById(scriptContainerId)
    val form = ConfigForm(
      logGen,
      onSolve(scriptContainer, computeContainerId, resultCanvasId))

    val container = divById(configId)
    container.innerHTML = ""
    container.appendChild(form.render)
  }

  def divById(id: String): Div = elmById(id) match {
    case div: Div => div
  }

  def canvasById(id: String): Canvas = elmById(id) match {
    case c: Canvas => c
  }

  def elmById(id: String) = document.getElementById(id)

  private def renderSolution(scriptContainer: Div,
                             resultCanvasId: String,
                             targetValue: Int,
                             solutionOpt: Option[Geneology[Equation]],
                             maxNodes: Int): Unit = {

    val canvas = canvasById(resultCanvasId)

    def windowHeight = (0.8 * window.innerHeight).toInt

    canvas.width = window.innerWidth.toInt
    canvas.height = windowHeight
    window.onresize = _ => {
      canvas.width = window.innerWidth.toInt
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

        val scriptContents =
          HtmlRenderer.javascriptCode(solutionNode, resultCanvasId)

        window.console.log(s"scriptContents is $scriptContents")
        scriptContainer.innerHTML = ""
        scriptContainer.appendChild(script(scriptContents).render)
      case None =>
        window.console.log("No solution")
        scriptContainer.innerHTML = "No Solution"
    }
  }

  private def computeSolution(scriptContainer: Div, resultCanvasId: String)(
      cfg: CountdownConfig,
      maxNodes: Int) = {

    window.console.info(
      s"$cfg to be rendered to $resultCanvasId using $maxNodes")
    val solution: Option[Geneology[Equation]] = cfg.solve()

    TransitionEvent.registerListener {
      case event: TransitionEnd if event.isSolutionTarget =>
        global.execute(() => {
          renderSolution(scriptContainer,
                         resultCanvasId,
                         cfg.targetValue,
                         solution,
                         maxNodes)
        })
        true
    }

    TransitionEvent.doJumpToSolutionFrame()
  }

  /**
    * Our 'onSolve' callback - instruct the UI to move to the 'compute' frame, then start computing...
    *
    * @param scriptContainer
    * @param resultCanvasId
    * @param cfg
    * @param maxNodes
    * @return
    */
  def onSolve(scriptContainer: Div,
              computeContainerId: String,
              resultCanvasId: String)(cfg: CountdownConfig, maxNodes: Int) = {

    // clear previous results
    scriptContainer.innerHTML = ""
    divById(computeContainerId).innerHTML = ""

    TransitionEvent.registerListener {
      case event: TransitionEnd if event.isComputeTarget =>
        global.execute(() => {
          computeSolution(scriptContainer, resultCanvasId)(cfg, maxNodes)
        })
        true
    }

    TransitionEvent.doJumpToComputeFrame()
  }
}
