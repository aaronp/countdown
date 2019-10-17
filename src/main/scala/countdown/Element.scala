package countdown

sealed trait Element
final case class Num(x: Int) extends Element
final case object Add extends Element
final case object Subtract extends Element
final case object Multiply extends Element
final case object Divide extends Element

object Element {
  def opForInt(x: Int): Element = {
    x match {
      case 0 => Add
      case 1 => Subtract
      case 2 => Multiply
      case _ => Divide
    }
  }
  def populate(inputs: Set[Int], nextInt: Int => Int): Seq[Int] = {
    def choose(values: IndexedSeq[Int], chosen: Seq[Int]): Seq[Int] = {
      values.size match {
        case 0 => chosen
        case 1 => values.head +: chosen
        case _ =>
          val index = nextInt(values.size - 1)
          val x = values(index)
          val remaining = values.filterNot(_ == x)
          choose(remaining, x +: chosen)
      }
    }
    choose(inputs.toIndexedSeq, Nil)
  }
  def eval(eq: Seq[Element]): Double = {
    eq match {
      case Seq(Num(x))                   => x
      case Num(x) +: Add +: theRest      => x + eval(theRest)
      case Num(x) +: Subtract +: theRest => x - eval(theRest)
      case Num(x) +: Multiply +: theRest => x * eval(theRest)
      case Num(x) +: Divide +: theRest   => x / eval(theRest)
    }
  }
}
