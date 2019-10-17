package countdown


sealed trait Geneology[A] {
  def value: A
}

object Geneology {
  implicit def ordering[A: Ordering]: Ordering[Geneology[A]] = {
    Ordering.by[Geneology[A], A](_.value)
  }
}

case class Origin[A](override val value: A) extends Geneology[A]

case class Offspring[A](override val value: A, generation: Int, offspringNr: Int, mom: Geneology[A], dad: Geneology[A]) extends Geneology[A]
case class Mutation[A](override val value: A, original : Offspring[A]) extends Geneology[A]

