package countdown

import cats.Show
import cats.data.State
import ga._

final case class Equation(expression: Seq[Element]) {
  def isSuccessful(targetNumber: Int): Boolean = {
    hasUniqueNumbers && eval.exists(_ == targetNumber)
  }

  def hasUniqueNumbers: Boolean = {
    val nums = numberComponents
    nums.size == nums.distinct.size
  }

  def numberComponents() = {
    expression.collect {
      case Num(x) => x
    }
  }

  /**
   * @param unsafeIndex
   * @return an equation at the shorted length
   */
  def truncate(unsafeIndex: Int) = {
    unsafeIndex.max(1) match {
      case n if n % 2 == 0 =>
        val index = (n - 1).max(1).min(size)
        copy(expression.take(index))
      case n =>
        val index = n.min(size)
        copy(expression.take(index))
    }

  }

  def mutateAt(index: Int, inputNumbers: Set[Int]): State[Seed, Equation] = {
    val elm: Element = expression(index)
    elm match {
      case op: Op =>
        Seed.nextInt(2).map { n =>
          val opIndex = (n + op.index) % Op.values.size
          val differentOp = Op
            .forInt(opIndex)
            .getOrElse(sys.error(
              s"Bug: bad opIndex $opIndex for rnd $n and $op (${op.index})"))
          swap(index, differentOp)
        }
      case Num(x) =>
        val remainingNumbers = inputNumbers - x
        if (remainingNumbers.isEmpty) {
          Seed.nextInt(1).map { _ =>
            Equation(Nil)
          }
        } else {
          Seed.nextInt(remainingNumbers.size - 1).map { numIdx =>
            val newNum = remainingNumbers.toSeq(numIdx)
            swap(index, Num(newNum))
          }
        }
    }
  }

  private def swap(index: Int, elm: Element): Equation = {
    val changed = expression.updated(index, elm)
    copy(expression = changed)
  }

  override def toString = s"$expressionString = ${eval.getOrElse("!")}"

  def expressionString = expression.mkString(" ")

  lazy val eval: Option[Int] = {
    Equation.reduce(expression).map(Equation.evalReduced)
  }

  def size: Int = expression.size

  def combineAt(other: Equation, index: Int): Equation = {
    val safeIndex = (size - 1).min(other.size - 1).min(index).max(0)
    safeIndex match {
      case 0 if expression.isEmpty => other
      case 0 if other.expression.isEmpty => this
      case 0 => other.swap(0, expression(0))
      case n =>
        copy(
          expression = expression.take(n + 1) ++ other.expression.drop(n + 1))
    }
  }

  def diff(targetNumber: Int) = {
    eval.map(x => (targetNumber - x).abs)
  }
}

object Equation {

  private[countdown] final def reduce(
                                       elements: Seq[Element]): Option[Seq[Element]] = {
    import cats.syntax.option._
    elements match {
      case Num(x) +: Divide +: Num(y) +: theRest =>
        if (y != 0 && x % y == 0) {
          reduce(Num(x / y) +: theRest)
        } else {
          Option.empty[Seq[Element]]
        }
      case Num(x) +: Multiply +: Num(y) +: theRest =>
        reduce(Num(x * y) +: theRest)
      case Num(x) +: op +: theRest => reduce(theRest).map(Num(x) +: op +: _)
      case seq => seq.some
    }
  }

  private[countdown] final def evalReduced(eq: Seq[Element]): Int = {
    eq match {
      case Seq() => Int.MinValue
      case Seq(Num(x)) => x
      case Num(x) +: Add +: theRest => x + evalReduced(theRest)
      case Num(x) +: Subtract +: Num(y) +: theRest =>
        evalReduced(Num(x - y) +: theRest)
    }
  }

  def showForTarget(targetNumber: Int): Show[Equation] = {
    Show.fromToString[Equation]
  }

  // order by difference
  def orderingForTarget(targetNumber: Int) = Ordering.by[Equation, Int] { eq =>
    val diff = eq.diff(targetNumber)
    // duplicate input numbers are invalid
    val opt = if (eq.hasUniqueNumbers) {
      val penalty = 10
      diff.map(_ * penalty)
    } else {
      diff
    }
    opt.getOrElse(Int.MaxValue)
  }

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

  def populate(values: Set[Int],
               popSize: Int,
               seed: Seed): (Seed, Seq[Equation]) = {
    populate(values, 2, values.size, popSize, seed)
  }

  def populate(values: Set[Int],
               minEqSize: Int,
               maxEqSize: Int,
               popSize: Int,
               seed: Seed) = {
    (0 to popSize).foldLeft((seed -> Seq[Equation]())) {
      case ((nextSeed, equations), _) =>
        val (newRnd, magnitude) =
          Seed.nextInt(maxEqSize - minEqSize).run(nextSeed).value
        val size = minEqSize + magnitude
        val (rnd, nextEq) = equationOfLen(size, values, newRnd)
        rnd -> (nextEq +: equations)
    }
  }

  def equationOfLen(size: Int,
                    fromValues: Set[Int],
                    seed: Seed): (Seed, Equation) = {
    require(fromValues.nonEmpty, "No input values specified")

    def next(valuePool: List[Int]) =
      for {
        index <- Seed.nextInt(valuePool.size - 1)
        value = valuePool(index)
        opIndex <- Seed.nextInt(3)
        op = Op.forInt(opIndex).getOrElse(sys.error(s"Bug: $opIndex"))
      } yield {
        (Num(value), op, valuePool diff List(value))
      }

    val (_, rnd, equation) = (1 to size.min(fromValues.size))
      .foldLeft((fromValues.toList, seed, Seq[Element]())) {
        case ((pool, rnd, result), _) =>
          val (newRnd, (num, op, newPool)) = next(pool).run(rnd).value
          (newPool, newRnd, num +: op +: result)
      }
    // drop the last operation
    rnd -> Equation(equation.init)
  }
}
