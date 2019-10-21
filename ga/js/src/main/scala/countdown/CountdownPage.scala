package countdown

import ga.GeneticAlgo.Generation
import ga.{Geneology, HtmlRenderer, Node}
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLTextAreaElement
import org.scalajs.dom.window
import scalatags.JsDom.all._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/**
  *
  * Contains some functions for creating a [[CountdownConfig]] from an HTML form, as well as a means
  * to display the compute/solution pages
  *
  * A big thanks to:
  * {{{
  * http://getspringy.com/
  * https://www.sanwebe.com/2014/08/css-html-forms-designs
  * https://www.scala-js.org/
  * https://swipe.js.org/
  * https://typelevel.org/cats/
  * }}}
  */
@JSExportTopLevel("CountdownPage")
object CountdownPage {

  @JSExport
  def render(configId: String,
             computeContainerId: String,
             scriptContainerId: String,
             resultCanvasId: String) = {

    val computeContainer = HtmlUtils.divById(computeContainerId)
    computeContainer.innerHTML = ""
    val logContainer =
      textarea(`class` := "logs", cols := 200, rows := 1000).render
    computeContainer.appendChild(logContainer)
    window.console.log(s"Appending textarea to $computeContainerId")

    // a function we pass in to log progress
    def logGeneration(generation: Generation[Equation]) = {
      val (gen, population) = generation
      val sep = "-" * 120
      val heading = s"Generation $gen:\n$sep\n"

      val content = population.mkString(s"$heading", "\n", "\n")
      window.console.info(content)

      logContainer.value += content
    }

    val scriptContainer = HtmlUtils.divById(scriptContainerId)
    val form = ConfigForm(
      logGeneration,
      onSolve(scriptContainer, computeContainerId, resultCanvasId))

    val container = HtmlUtils.divById(configId)
    container.innerHTML = ""
    container.appendChild(form.render)
  }

  private def renderSolution(scriptContainer: Div,
                             resultCanvasId: String,
                             targetValue: Int,
                             solutionOpt: Option[Geneology[Equation]],
                             maxNodes: Int): Unit = {

    val canvas = HtmlUtils.canvasById(resultCanvasId)

    // let's render the height as less than the full window height
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

  private def clearResults(scriptContainer: Div,
                           computeContainerId: String,
                           resultCanvasId: String) = {
    scriptContainer.innerHTML = ""

    val workingsOutTextArea = HtmlUtils
      .childrenFor(HtmlUtils.divById(computeContainerId))
      .collect {
        case ta: HTMLTextAreaElement => ta
      }
    workingsOutTextArea.foreach(_.value = "")

    val canvas = HtmlUtils.canvasById(resultCanvasId)
    val context = canvas.getContext("2d")
    context.clearRect(0, 0, canvas.width * 2, canvas.height * 2)
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
    clearResults(scriptContainer, computeContainerId, resultCanvasId)

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
