package countdown

import ga.GeneticAlgo.Generation
import ga.Seed
import org.scalajs.dom.html.Form
import org.scalajs.dom.window
import scalatags.JsDom
import scalatags.JsDom.all.{`class`, _}

/**
  * Rendering of our configuration form
  */
object ConfigForm {

  def apply(logGeneration: Generation[Equation] => Unit,
            onSolve: (CountdownConfig, Int) => Unit): JsDom.TypedTag[Form] = {
    import FormElement.configListItems

    val submit = input(`type` := "submit", value := "Solve").render

    submit.onclick = e => {
      e.preventDefault()
      e.stopPropagation()

      val resOpt = for {
        (seed, settings) <- configListItems.asSettings
        inputNrs <- configListItems.inputNumbers.currentValue()
        targetValue <- configListItems.targetNr.currentValue()
        minEqSize <- configListItems.minEquationSize.currentValue()
        maxNodes <- configListItems.maxNodes.currentValue()
      } yield {
        val rnd = seed.getOrElse(System.currentTimeMillis())
        // stamp our random seed
        FormElement.configListItems.seed.inputElement.value = rnd.toString
        window.console.info(s"settings is $settings")

        val countdownCfg = new CountdownConfig(
          settings = settings,
          rand = Seed(rnd),
          inputValues = inputNrs,
          targetValue = targetValue,
          logGeneration,
          minEquationSize = minEqSize,
          maxEquationSize = inputNrs.size,
          None
        )

        window.console.info(s"CountdownConfig is $countdownCfg")

        onSolve(countdownCfg, maxNodes)
      }

      if (resOpt.isEmpty) {
        val invalidFields = configListItems.elements.flatMap { elm =>
          elm.currentValueEither match {
            case Left(msg) => Option(elm.field -> msg)
            case Right(_)  => None
          }
        }
        invalidFields match {
          case Seq() =>
          case Seq((field, msg)) =>
            window.alert(s"${field} is invalid: $msg")
          case many =>
            val msg = many
              .map {
                case (field, msg) => s"$field: $msg"
              }
              .mkString(s"${many.size} invalid fields:\n\n", "\n\n", "")

            window.alert(msg)
        }

      }
    }

    form(`class` := "form-style-7")(
      ul(
        configListItems.targetNr.liElement,
        configListItems.inputNumbers.liElement,
        configListItems.seed.liElement,
        configListItems.maxGen.liElement,
        configListItems.populationSize.liElement,
        configListItems.mutationProbability.liElement,
        configListItems.minEquationSize.liElement,
        configListItems.maxNodes.liElement,
        li(submit)
      ))
  }

}
