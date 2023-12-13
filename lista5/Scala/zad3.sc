def stirling(n: Int) (m: Int): BigInt =

  if n > 0 && m > 0 then
    (n, m) match {
      case (0, 0) => 1
      case (_, 1) => 1
      case (_, 0) => 0
      case (a, b) if a == b => 1
      case _ => stirling(n - 1) (m - 1) + m * stirling(n - 1) (m)
    }
  else throw new IllegalArgumentException("invalid arguments")

lazy val lazyStirling: BigInt ={ println("aha"); stirling(32) (16)}

println("Wartość leniwej zmiennej:")

println(lazyStirling)
println(lazyStirling)
println(lazyStirling)
println(lazyStirling)
println(lazyStirling)