import scala.collection.mutable.HashMap

def makeMemoize[A, B](funToMemoize: A => B): A => B = {
  val memoTable = new HashMap[A, B]()

  (x: A) => memoTable.getOrElseUpdate(x, funToMemoize(x))
}



def expensiveFunction(n: Int): Int = {
  println("expensiveFunction called")
  Thread.sleep(5000)
  n * n
}


val expensiveFunctionMemoized = makeMemoize(expensiveFunction)
println("start")
println(expensiveFunctionMemoized(5))
println(expensiveFunctionMemoized(5))
println(expensiveFunctionMemoized(5))
println(expensiveFunctionMemoized(5))




