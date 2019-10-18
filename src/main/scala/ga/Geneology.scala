package ga

import cats.Show
import cats.syntax.show._

sealed trait Geneology[A] {
  def value: A

}

object Geneology {
  implicit def ordering[A: Ordering]: Ordering[Geneology[A]] = {
    Ordering.by[Geneology[A], A](_.value)
  }
}

case class Origin[A : Show](override val value: A) extends Geneology[A] {
  override def toString: String = value.show
}

case class Offspring[A : Show](override val value: A, generation: Int, offspringNr: Int, mom: Geneology[A], dad: Geneology[A]) extends Geneology[A] {
  override def toString: String = s"gen $generation, o $offspringNr: ${value.show}"
}
case class Mutation[A: Show](override val value: A, original : Offspring[A]) extends Geneology[A] {
  override def toString: String = s"mutate $original -> ${value.show}"
}

