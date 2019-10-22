package countdown

import ga.AlgoSettings
import scalatags.JsDom.all.{value, _}

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

final case class FormElement[A] private (
    field: String,
    initialValue: String,
    hint: String,
    validate: String => Either[String, A]) {
  val fieldId = field.filter(_.isLetterOrDigit).toLowerCase

  val inputElement = input(`type` := "text",
                           name := fieldId,
                           maxlength := "100",
                           value := initialValue).render

  inputElement.onkeyup = _ => {
    validateCurrentValue()
  }

  def validateCurrentValue() = {
    validate(textValue) match {
      case Left(msg) => validationSpan.innerHTML = msg
      case Right(_)  => validationSpan.innerHTML = ""
    }
  }

  private val validationSpan = span(style := "color:orange !important").render
  val liElement = li(
    label(`for` := fieldId)(field),
    inputElement,
    span(hint),
    validationSpan,
  )

  def currentValue(): Option[A] = currentValueEither().toOption

  def currentValueEither(): Either[String, A] = validate(textValue)

  private def textValue = inputElement.value
}

object FormElement {

  private def valueAsNumbers(textValue: String): Try[Set[Int]] = {
    Try(textValue.split(",").flatMap(_.split(" ")).map(_.toInt).toSet)
  }

  // our poor-man's enum 'values()' for the form
  private val formElements = ListBuffer[FormElement[_]]()

  private def newFormElement[A](field: String,
                                initialValue: String,
                                hint: String,
                                validate: String => Either[String, A]) = {
    val elm = new FormElement[A](field, initialValue, hint, validate)
    formElements += elm
    elm
  }

  def int(field: String,
          initialValue: String,
          hint: String,
          min: Int = 0,
          max: Int = 500) = {
    def asInt(value: String) = Try(value.trim.toInt) match {
      case Failure(_)                         => Left("invalid integer")
      case Success(a) if a >= min && a <= max => Right(a)
      case Success(a)                         => Left(s"$a should be between ${min} and $max")
    }

    newFormElement(field, initialValue, hint, asInt)
  }

  def double(field: String,
             initialValue: String,
             hint: String,
             min: Double,
             max: Double) = {
    def asInt(value: String) = Try(value.trim.toDouble) match {
      case Failure(_)                         => Left("invalid double")
      case Success(a) if a >= min && a <= max => Right(a)
      case Success(a)                         => Left(s"$a should be between ${min} and $max")
    }

    newFormElement(field, initialValue, hint, asInt)
  }

  def optLong(field: String,
              initialValue: String,
              hint: String): FormElement[Option[Long]] = {
    def asInt(value: String) = Try(value.trim.toLong) match {
      case Failure(_) if value.trim.isEmpty => Right(Option.empty[Long])
      case Failure(_)                       => Left("invalid integer")
      case Success(a)                       => Right(Option(a))
    }

    newFormElement(field, initialValue, hint, asInt)
  }

  def ints(field: String, initialValue: String, hint: String) = {
    def asInt(value: String) = valueAsNumbers(value.trim) match {
      case Failure(_) =>
        Left("required a space or comma-separated list of integers")
      case Success(a) => Right(a)
    }

    newFormElement[Set[Int]](field, initialValue, hint, asInt)
  }

  object configListItems {

    def elements: List[FormElement[_]] = formElements.toList

    val targetNr =
      FormElement.int("Target Number",
                      "378",
                      "The number we're trying to find",
                      0,
                      1000000000)
    val inputNumbers =
      FormElement.ints("Using Input Numbers",
                       "25 3 8 12 9 15",
                       "The numbers available to reach the target number")
    val seed = FormElement.optLong(
      "Random Value Seed",
      "",
      "An optional value with which to initialise our random value generator")
    val maxGen = FormElement.int(
      "Max Generations",
      "60",
      "How many generations to allow before we quit without an answer",
      2,
      5000)
    val populationSize = FormElement.int(
      "Population Size",
      "200",
      "How large we should allow the population to grow")

    val mutationProbability = FormElement.double(
      "Mutation Probability",
      "0.01",
      "A number between 0.0 and 1.0 representing the probability of a mutation",
      0.0,
      1.0)

    val minEquationSize = {
      val base = FormElement.int(
        "Minimum Equation Size",
        "3",
        "The smallest equation length to use in the initial population")

      def checkInputs(str: String) = {
        base.validate(str) match {
          case right @ Right(size) =>
            val maxOpt = inputNumbers.currentValue().map(_.size)
            val minSizeLessThanAvailNrs = maxOpt.exists(_ >= size)
            if (minSizeLessThanAvailNrs) {
              right
            } else {
              Left(
                s"The minimum equation size needs to be fewer than the number of available unique inputs (e.g. ${maxOpt
                  .getOrElse(0)})")
            }
          case left => left
        }
      }

      base.copy(validate = checkInputs)
    }

    val maxNodes =
      FormElement.int(
        "Max Solution Nodes to Display",
        "20",
        "The maximum number of nodes to show when displaying our solution graph",
        1,
        50)

    def asSettings: Option[(Option[Long], AlgoSettings[Equation])] = {
      for {
        targetNumber <- targetNr.currentValue()
        inputNumbers <- inputNumbers.currentValue()
        maxPopulation <- populationSize.currentValue()
        mutationProbability <- mutationProbability.currentValue()
        maxGenerations <- maxGen.currentValue()
        seed <- seed.currentValue()
      } yield {

        val settings = CountdownConfig.makeAlgoSettings(
          targetNumber = targetNumber,
          inputNumbers = inputNumbers,
          maxPopulation = maxPopulation,
          mutationProbability = mutationProbability,
          maxGenerations = maxGenerations
        )
        (seed, settings)
      }
    }
  }

}
