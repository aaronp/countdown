package ga

import cats.Show
import cats.syntax.show._

sealed trait Geneology[A] {
  def value: A
}

case class Origin[A: Show](override val value: A) extends Geneology[A]

case class Offspring[A: Show](override val value: A,
                              generation: Int,
                              offspringNr: Int,
                              mom: Geneology[A],
                              dad: Geneology[A])
    extends Geneology[A] {
  override def toString: String =
    s"gen $generation, o $offspringNr: ${value.show}"
}
case class Mutation[A: Show](override val value: A, original: Offspring[A])
    extends Geneology[A] {
  override def toString: String = s"mutate $original -> ${value.show}"
}
