package ga

import cats.Show
import cats.syntax.show._

sealed trait Geneology[A] {
  def value: A

  def render(indent: Int): String

  override def toString = render(0)

  protected def indent(n: Int): String = " " * n


}

object Geneology {
  implicit def ordering[A: Ordering]: Ordering[Geneology[A]] = {
    Ordering.by[Geneology[A], A](_.value)
  }
}

case class Origin[A: Show](override val value: A) extends Geneology[A] {
  override def render(offset: Int) = s"""${indent(offset)}Origin: $value""".stripMargin
}

case class Offspring[A: Show](override val value: A, generation: Int, offspringNr: Int, mom: Geneology[A], dad: Geneology[A]) extends Geneology[A] {
  override def render(offset: Int): String = {
    val lhs = mom.render(offset + 6)
    val rhs = dad.render(offset + 6)
    s"""${indent(offset)}Generation $generation:
       |${indent(offset)}  Value #${offspringNr} : ${value.show}
       |${indent(offset)}  Based on:
       |${indent(offset)}  Mom
       |$lhs
       |${indent(offset)}  Dad
       |$rhs
       |
       |""".stripMargin
  }

}

case class Mutation[A: Show](override val value: A, original: Offspring[A]) extends Geneology[A] {
  override def render(offset: Int) = {

    val child = original.render(offset + 4)
    val width = child.linesIterator.map(_.length).max
    val line = "-" * width

    s"""${indent(offset)}Mutate:
       |${indent(offset)}  $value
       |${indent(offset)}  to
       |$child
       |${indent(offset)}$line
       |""".stripMargin
  }

}

