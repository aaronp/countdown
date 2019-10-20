package countdown

/**
  * Represents either a number or an operation in the countdown space
  */
sealed trait Element

final case class Num(x: Int) extends Element {
  override def toString = x.toString
}

abstract class Op(override val toString: String, val index: Int) extends Element

object Op {

  def values: Set[Op] = Set(Add, Subtract, Multiply, Divide)
  def forInt(x: Int): Option[Op] = {
    values.find(_.index == x)
  }
}

final case object Add extends Op("+", 0)

final case object Subtract extends Op("-", 1)

final case object Multiply extends Op("*", 2)

final case object Divide extends Op("/", 3)
