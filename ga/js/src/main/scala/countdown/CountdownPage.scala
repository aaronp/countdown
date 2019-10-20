package countdown

import org.scalajs.dom.document
import org.scalajs.dom.html.Div
import scalatags.JsDom.all.{`class`, _}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("CountdownPage")
object CountdownPage {

  def divById(id: String): Div = elmById(id) match {
    case div: Div => div
  }

  def elmById(id: String) = document.getElementById(id)

  // thansk 'https://www.sanwebe.com/2014/08/css-html-forms-designs'
  def configForm() = {

    def makeLi(field: String, hint: String) = {
      val fieldId = field.filter(_.isLetterOrDigit)

      li(
        label(`for` := fieldId)(field),
        input(`type` := "text", name := fieldId, maxlength := "100"),
        span(hint)
      )
    }

    form(`class` := "form-style-7")(
      ul(
        makeLi("Target Number", "The number we're trying to find"),
        makeLi("Using", "The input numbers we can use")
      ))
  }

  @JSExport
  def render(configId: String) = {

    val container = divById(configId)
    container.innerHTML = ""
    container.appendChild(configForm.render)
  }
}
