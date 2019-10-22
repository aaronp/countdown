package countdown

import ga.Geneology
import ga.GeneticAlgo.Generation
import org.scalajs.dom.html.{Div, Paragraph}
import org.scalajs.dom.raw.HTMLTextAreaElement
import org.scalajs.dom.window
import scalatags.JsDom.all._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.Try

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

  private var logContent = ListBuffer[String]()

  @JSExport
  def render(configId: String,
             computeContainerId: String,
             scriptContainerId: String,
             resultCanvasId: String) = {

    val computeContainer = HtmlUtils.divById(computeContainerId)
    computeContainer.innerHTML = ""
    val logContainer =
      textarea(`class` := "logs", cols := 200, rows := 400).render
    val calculatingMsg: Paragraph = p("Calculating ").render
    computeContainer.appendChild(calculatingMsg)
    computeContainer.appendChild(logContainer)

    // a function we pass in to log progress
    def logGeneration(generation: Generation[Equation]) = {
      val (gen, population) = generation
      val sep = "-" * 120
      val header = s"\n$sep\nGeneration $gen:\n"

      val content = population.mkString(header, "\n", "")
      logContent += content
      window.console.info(content)
      logContainer.value = logContent.mkString("\n")
      logContainer.scrollTop = logContainer.scrollHeight
    }

    val scriptContainer = HtmlUtils.divById(scriptContainerId)
    val form = ConfigForm(logGeneration,
                          onSolve(scriptContainer,
                                  computeContainerId,
                                  resultCanvasId,
                                  calculatingMsg))

    val container = HtmlUtils.divById(configId)
    container.innerHTML = ""
    container.appendChild(form.render)
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
          ForcedLayoutSolution.renderSolution(scriptContainer,
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
    window.console.log("Clearing results")
    scriptContainer.innerHTML = ""

    logContent.clear()

    val workingsOutTextArea = HtmlUtils
      .childrenFor(HtmlUtils.divById(computeContainerId))
      .collect {
        case ta: HTMLTextAreaElement => ta
      }
    workingsOutTextArea.foreach(_.value = "")

    // defined in countdown.js
    Try(js.Dynamic.global.clearGraph())

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
  def onSolve(
      scriptContainer: Div,
      computeContainerId: String,
      resultCanvasId: String,
      calculatingMsg: Paragraph)(cfg: CountdownConfig, maxNodes: Int) = {

    // clear previous results
    clearResults(scriptContainer, computeContainerId, resultCanvasId)

    calculatingMsg.innerHTML =
      s"Calculating ${cfg.targetValue} from ${cfg.inputValues.toSeq.sorted.mkString(" ")} ..."
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
