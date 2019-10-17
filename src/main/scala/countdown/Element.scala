package countdown

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

  type Equation = Seq[Element]

  def populate(values: Set[Int], minEqSize: Int, maxEqSize: Int, popSize: Int, seed: Seed) = {
    (0 to popSize).foldLeft((seed -> Seq[Equation]())) {
      case ((nextSeed, equations), _) =>
        val (newRnd, mangitude) = Seed.nextInt(maxEqSize - minEqSize).run(nextSeed).value
        val size = minEqSize + mangitude
        val (rnd, nextEq) = equationOfLen(size, values, newRnd)
        rnd -> (nextEq +: equations)
    }
  }

  def equationOfLen(size: Int, fromValues: Set[Int], seed: Seed): (Seed, Seq[Element]) = {
    def next(valuePool: List[Int]) = for {
      index <- Seed.nextInt(valuePool.size - 1)
      value = valuePool(index)
      opIndex <- Seed.nextInt(4)
      op = opForInt(opIndex)
    } yield {
      (Num(value), op, valuePool diff List(value))
    }

    val (_, rnd, equation) = (1 to size.min(fromValues.size)).foldLeft((fromValues.toList, seed, Seq[Element]())) {
      case ((pool, rnd, result), _) =>
        val (newRnd, (num, op, newPool)) = next(pool).run(rnd).value
        (newPool, newRnd, num +: op +: result)
    }
    // drop the last operation
    rnd -> equation.init
  }

  def opForInt(x: Int): Element = {
    x match {
      case 0 => Add
      case 1 => Subtract
      case 2 => Multiply
      case _ => Divide
    }
  }

//  def populate(inputs: Set[Int], nextInt: Int => Int): Seq[Int] = {
//    def choose(values: IndexedSeq[Int], chosen: Seq[Int]): Seq[Int] = {
//      values.size match {
//        case 0 => chosen
//        case 1 => values.head +: chosen
//        case _ =>
//          val index = nextInt(values.size - 1)
//          val x = values(index)
//          val remaining = values.filterNot(_ == x)
//          choose(remaining, x +: chosen)
//      }
//    }
//
//    choose(inputs.toIndexedSeq, Nil)
//  }

  def eval(eq: Seq[Element]): Double = {
    eq match {
      case Seq(Num(x)) => x
      case Num(x) +: Add +: theRest => x + eval(theRest)
      case Num(x) +: Subtract +: theRest => x - eval(theRest)
      case Num(x) +: Multiply +: theRest => x * eval(theRest)
      case Num(x) +: Divide +: theRest => x / eval(theRest)
    }
  }
}
