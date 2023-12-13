import scala.collection.mutable.HashMap

def stirling(n: Int) (m: Int): BigInt =
  if n >= 0 && m >= 0 then
    (n, m) match {
      case (0, 0) => 1
      case (_, 1) => 1
      case (_, 0) => 0
      case (a, b) if a == b => 1
      case _ => stirling(n - 1) (m - 1) + m * stirling(n - 1) (m)
    }
  else throw new IllegalArgumentException("invalid arguments")


def memoizedStirling(n: Int) (m: Int): BigInt = {
  if (n >= 0 && m >= 0) {
    val cache = new HashMap[(Int, Int), BigInt]()

    def stirling(n: Int) (m: Int): BigInt = {
      cache.getOrElseUpdate((n, m), {
        (n, m) match {
          case (0, 0) => 1
          case (_, 1) => 1
          case (_, 0) => 0
          case (a, b) if a == b => 1
          case _ => stirling(n - 1) (m - 1) + m * stirling(n - 1) (m)
        }
      })
    }
    stirling(n)(m)
  } else {
    throw new IllegalArgumentException("invalid arguments")
  }
}

println("memomized")
memoizedStirling(33) (15)
println("non-memomized")
stirling(33)(15)








