package countdown

import cats.Show
import cats.data.State
import cats.kernel.Semigroup
import ga._

final case class Equation(expression: Seq[Element]) {

  /**
   * Alter this equation, changing the value at 'index'
   *
   * @param index
   * @param inputNumbers
   * @return
   */
  def mutateAt(index: Int, inputNumbers: Set[Int]): State[Seed, Equation] = {
    expression(index) match {
      case op: Op =>
        Seed.nextInt(2).map { n =>
          val opIndex = (n + op.index) % Op.values.size
          val differentOp = Op.forInt(opIndex).getOrElse(sys.error(s"Bug: bad opIndex $opIndex for rnd $n and $op (${op.index})"))
          swap(index, differentOp)
        }
      case Num(x) =>
        Seed.nextInt(inputNumbers.size - 1).map { index =>
          val newNum = inputNumbers.toSeq(index)
          swap(index, Num(newNum))
        }
    }
  }

  private def swap(index: Int, elm: Element): Equation = {
    val changed = expression.updated(index, elm)
    copy(expression = changed)
  }

  override def toString = {
    expression.mkString("", " ", " == " + eval)
  }

  lazy val eval: Option[Int] = Equation.evalDouble(expression).map(_.toInt)

  def mutate() = {
    this
  }

  def size = expression.size

  def combineAt(other: Equation, index: Int): Equation = {
    val maxSize = size.min(other.size)

    copy(expression = other.expression.take(index) ++ other.expression.drop(index))
  }

  def diff(targetNumber: Int): Int = {
    eval.map(x => (targetNumber - x).abs).getOrElse(Int.MaxValue)
  }
}

object Equation {

  private final def evalDouble(eq: Seq[Element]): Option[Double] = {
    eq match {
      case Seq(Num(x)) => Option(x)
      case Num(x) +: Add +: theRest => evalDouble(theRest).map(x + _)
      case Num(x) +: Subtract +: theRest => evalDouble(theRest).map(x - _)
      case Num(x) +: Multiply +: Num(y) +: theRest => evalDouble(Num(x * y) +: theRest)
      case Num(x) +: Divide +: Num(y) +: theRest =>
        if (x % y != 0) {
          None
        } else {
          evalDouble(Num(x / y) +: theRest)
        }
    }
  }

  def equationSemigroup(index: Int): Semigroup[Equation] = Semigroup.instance[Equation] {
    case (a, b) => a.combineAt(b, index)
  }

  // order by difference
  def orderingForTarget(targetNumber: Int) = Ordering.by[Equation, Int] { eq =>
    eq.diff(targetNumber)
  }

  implicit val show = Show.fromToString[Equation]

  def parse(value: String): Equation = {
    val expression = value.split(" ").map {
      case "+" => Add
      case "-" => Subtract
      case "*" => Multiply
      case "/" => Divide
      case i => Num(i.toInt)
    }
    new Equation(expression)
  }

  def populate(values: Set[Int], minEqSize: Int, maxEqSize: Int, popSize: Int, seed: Seed) = {
    (0 to popSize).foldLeft((seed -> Seq[Equation]())) {
      case ((nextSeed, equations), _) =>
        val (newRnd, mangitude) = Seed.nextInt(maxEqSize - minEqSize).run(nextSeed).value
        val size = minEqSize + mangitude
        val (rnd, nextEq) = equationOfLen(size, values, newRnd)
        rnd -> (nextEq +: equations)
    }
  }

  def equationOfLen(size: Int, fromValues: Set[Int], seed: Seed): (Seed, Equation) = {
    def next(valuePool: List[Int]) = for {
      index <- Seed.nextInt(valuePool.size - 1)
      value = valuePool(index)
      opIndex <- Seed.nextInt(3)
      op = Op.forInt(opIndex).getOrElse(sys.error(s"Bug: $opIndex"))
    } yield {
      (Num(value), op, valuePool diff List(value))
    }

    val (_, rnd, equation) = (1 to size.min(fromValues.size)).foldLeft((fromValues.toList, seed, Seq[Element]())) {
      case ((pool, rnd, result), _) =>
        val (newRnd, (num, op, newPool)) = next(pool).run(rnd).value
        (newPool, newRnd, num +: op +: result)
    }
    // drop the last operation
    rnd -> Equation(equation.init)
  }
}