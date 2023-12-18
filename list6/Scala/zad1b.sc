enum Expression:
  case Val(value: Double)
  case Sum
  case Diff
  case Prod
  case Div

def evaluate(elements: List[Expression]): Option[Double] = {
  def applyOperation(operator: Expression, operand1: Double, operand2: Double): Option[Double] = {
    operator match
      case Expression.Sum => Some(operand1 + operand2)
      case Expression.Diff => Some(operand1 - operand2)
      case Expression.Prod => Some(operand1 * operand2)
      case Expression.Div =>
        if operand2 != 0 then Some(operand1 / operand2)
        else None
      case _ => None
  }

  @scala.annotation.tailrec
  def evaluateHelper(stack: List[Double], remainingElements: List[Expression]): Option[Double] = {
    (remainingElements, stack) match
      case (Nil, head :: Nil) => Some(head)
      case (Expression.Val(value) :: tail, _) => evaluateHelper(value :: stack, tail)
      case (operation :: tail, operand1 :: operand2 :: rest) =>
        applyOperation(operation, operand1, operand2) match
          case Some(result) => evaluateHelper(result :: rest, tail)
          case None => None
      case _ => None
  }

  evaluateHelper(List(), elements)
}

// Tests using the Expression enum:
println(evaluate(List(Expression.Val(1), Expression.Val(3.5), Expression.Sum))) // Some(4.5)
println(evaluate(List(Expression.Val(1), Expression.Val(0), Expression.Div))) // Some(0.0)
println(evaluate(List(Expression.Val(2), Expression.Val(3), Expression.Sum, Expression.Val(4), Expression.Prod))) // Some(20.0)
println(evaluate(List(Expression.Val(2), Expression.Val(3), Expression.Prod, Expression.Val(4), Expression.Sum))) // Some(10.0)
println(evaluate(List(Expression.Val(2), Expression.Val(3), Expression.Sum, Expression.Val(4), Expression.Prod, Expression.Val(5), Expression.Div, Expression.Val(2), Expression.Diff))) // Some(1.75)
println(evaluate(List(Expression.Val(1), Expression.Val(3.5)))) // None
println(evaluate(List(Expression.Val(1), Expression.Val(3.5), Expression.Sum, Expression.Prod))) // None
println(evaluate(List(Expression.Val(0), Expression.Val(1), Expression.Div))) // None
println(evaluate(List())) // None
