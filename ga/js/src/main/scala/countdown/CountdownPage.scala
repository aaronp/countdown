package countdown

import ga.GeneticAlgo.Generation
import ga.Seed
import org.scalajs.dom.html.Div
import org.scalajs.dom.{document, window}
import scalatags.JsDom.all.{`class`, _}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scala.util.Try

@JSExportTopLevel("CountdownPage")
object CountdownPage {

  def divById(id: String): Div = elmById(id) match {
    case div: Div => div
  }

  def elmById(id: String) = document.getElementById(id)

  // thanks 'https://www.sanwebe.com/2014/08/css-html-forms-designs'
  def onSolve(cfg: CountdownConfig) = {
    cfg.solve() match {
      case Some(soln) =>
      case None       =>
    }
  }

  def configForm() = {

    def makeLi(field: String, initialValue: String, hint: String) = {
      val fieldId = field.filter(_.isLetterOrDigit).toLowerCase

      val inp = input(`type` := "text",
                      name := fieldId,
                      maxlength := "100",
                      value := initialValue).render
      val liElm = li(
        label(`for` := fieldId)(field),
        inp,
        span(hint)
      )
      (liElm, inp)
    }

    val (tgtNrLi, tgtNr) =
      makeLi("Target Number", "100", "The number we're trying to find")
    val (usingLi, using) =
      makeLi("Using", "1,2,3", "The input numbers we can use")
    val (seedLi, seed) = makeLi("Seed", "", "Our random number seed - optional")
    val (maxGenLi, maxGen) = makeLi(
      "Max Generations",
      "200",
      "How many generations to allow before we quit without an answer")
    val (popSizeLi, popSize) = makeLi(
      "Population Size",
      "100",
      "How large we should allow the population to grow")

    val (mutationProbLi, mutationProb) = makeLi(
      "Mutation Probability",
      "0.001",
      "A number between 0.0 and 1.0 representing the probability of a mutation")

    val (minEqSizeLi, minEqSize) = makeLi(
      "Minimum Equation Size",
      "6",
      "The smallest equation length to use in the initial population")

    def inputNumbers = {
      Try(using.value.split(",").flatMap(_.split(" ")).map(_.toInt).toSet)
    }

    using.onkeyup = _ => {
      inputNumbers.foreach { nrs =>
        Try(minEqSize.value.toInt).foreach { minSize =>
          if (minSize >= nrs.size) {
            minEqSize.value = nrs.size.toString
          }
        }
      }
    }

    val submit = input(`type` := "submit", value := "Solve").render
    submit.onclick = e => {
      window.console.info(s"Compute...")

      val targetNumber = tgtNr.value.toInt

      val settings = CountdownConfig.makeAlgoSettings(
        targetNumber = targetNumber,
        inputNumbers = inputNumbers.getOrElse(Set.empty),
        maxPopulation = popSize.value.toInt,
        mutationProbability = mutationProb.value.toDouble,
        maxGenerations = maxGen.value.toInt
      )

      val rnd = {
        val lngValue =
          Try(seed.value.toLong).getOrElse(System.currentTimeMillis)
        Seed(lngValue)
      }
      window.console.info(s"settings is $settings")

      def logGen(eq: Generation[Equation]) = {
        window.console.info(s"$eq")
      }

      val countdownCfg = new CountdownConfig(
        settings = settings,
        rand = rnd,
        inputValues = inputNumbers.getOrElse(Set.empty),
        targetValue = targetNumber,
        logGen,
        minEquationSize = minEqSize.value.toInt,
        maxEquationSize = inputNumbers.getOrElse(Set.empty).size,
        None
      )
      onSolve(countdownCfg)
    }

    form(`class` := "form-style-7")(
      ul(
        tgtNrLi,
        usingLi,
        seedLi,
        maxGenLi,
        popSizeLi,
        mutationProbLi,
        minEqSizeLi,
        li(submit)
      ))
  }

  @JSExport
  def render(configId: String) = {

    val container = divById(configId)
    container.innerHTML = ""
    container.appendChild(configForm.render)
  }
}
