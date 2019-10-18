package ga

/**
 * Represents either a number or an operation in the countdown space
 */
sealed trait Element

final case class Num(x: Int) extends Element {
  override def toString = x.toString
}

abstract class Op(override val toString: String) extends Element

final case object Add extends Op("+")

final case object Subtract extends Op("-")

final case object Multiply extends Op("*")

final case object Divide extends Op("/")

object Element {

  def opForInt(x: Int): Element = {
    x match {
      case 0 => Add
      case 1 => Subtract
      case 2 => Multiply
      case _ => Divide
    }
  }


  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
}
