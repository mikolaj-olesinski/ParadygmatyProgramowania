sealed trait Expression

case class Val(value: Double) extends Expression
case class Fun(function: (Double, Double) => Double) extends Expression
case object Sum extends Expression
case object Diff extends Expression
case object Prod extends Expression
case object Div extends Expression

def evaluate(elements: List[Expression]): Option[Double] = {
  def applyOperation(operator: Expression, operand1: Double, operand2: Double): Option[Double] = {
    operator match
      case Sum => Some(operand1 + operand2)
      case Diff => Some(operand1 - operand2)
      case Prod => Some(operand1 * operand2)
      case Div =>
        if operand2 != 0 then Some(operand1 / operand2) else None
      case Fun(function) => Some(function(operand1, operand2))
      case _ => None
  }

  @scala.annotation.tailrec
  def evaluateHelper(remainingElements: List[Expression], stack: List[Double]): Option[Double] = {
    (remainingElements, stack) match
      case (Nil, head :: Nil) => Some(head)
      case (Val(value) :: tail, _) => evaluateHelper(tail, value :: stack)
      case (operation :: tail, operand1 :: operand2 :: rest) =>
        applyOperation(operation, operand1, operand2) match
          case Some(result) => evaluateHelper(tail, result :: rest)
          case None => None

      case _ => None

  }


  evaluateHelper(elements, List())
}

// Przykłady użycia:
println(evaluate(List(Val(1), Val(3), Fun((x, y) => x + y * y)))) // Some(4.0)
println(evaluate(List(Val(0), Val(1), Fun((x, y) => x - y)))) //Some (1.0)
println(evaluate(List(Val(1), Val(3.5), Sum))) // Some(4.5)
println(evaluate(List(Val(1), Val(0), Div))) // Some(0.0)
println(evaluate(List(Val(2), Val(3), Sum, Val(4), Prod))) // Some(20.0)
println(evaluate(List(Val(2), Val(3), Prod, Val(4), Sum))) // Some(10.0)
println(evaluate(List(Val(2), Val(3), Sum, Val(4), Prod, Val(5), Div, Val(2), Diff))) // Some(1.75)


println(evaluate(List(Val(1), Val(3.5)))) // None
println(evaluate(List(Val(1), Val(3.5), Sum, Prod))) // None
println(evaluate(List(Val(0), Val(1), Div))) // None
println(evaluate(List())) // None

