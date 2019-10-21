package countdown

import org.scalajs.dom.html.Div
import org.scalajs.dom.window

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

/**
  *
  * @param index the index of the 'div' within the sliding section in index.html.
  * @param content
  */
sealed abstract class TransitionEvent(index: Int, content: Div) {
  def isAboutTarget = index == 0

  def isSetupTarget = index == 1

  def isComputeTarget = index == 2

  def isSolutionTarget = index == 3
}

@JSExportTopLevel("TransitionEvent")
object TransitionEvent {

  def doJumpToSolutionFrame() = {
    js.Dynamic.global.jumpToSolutionFrame()
  }

  def doJumpToComputeFrame() = {
    js.Dynamic.global.jumpToComputeFrame()
  }

  type Handler = PartialFunction[TransitionEvent, Boolean]
  private var transitionListeners = List[Handler]()

  def registerListener(handler: Handler): Handler = {
    transitionListeners = handler +: transitionListeners
    handler
  }

  @JSExport
  def onTransitionEnd(index: Int, element: Div): Unit = {
    window.console.info(s"onTransitionEnd($index, $element)")
    onTransition(TransitionEnd(index, element))
  }

  @JSExport
  def onTransitionStart(index: Int, element: Div): Unit = {
    window.console.info(s"onTransitionStart($index, $element)")
    onTransition(TransitionStart(index, element))
  }

  private def onTransition(event: TransitionEvent): Unit = {
    val remaining = transitionListeners.filter { handler =>
      val keep = !handler.isDefinedAt(event) || !handler(event)
      keep
    }
    transitionListeners = remaining
  }
}

final case class TransitionStart(index: Int, content: Div)
    extends TransitionEvent(index, content)

final case class TransitionEnd(index: Int, content: Div)
    extends TransitionEvent(index, content)
