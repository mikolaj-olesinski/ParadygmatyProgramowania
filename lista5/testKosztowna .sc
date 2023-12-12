val primes =
  def sieve(lxs: LazyList[Int]): LazyList[Int] =
    val p #:: xs = lxs
    p #:: sieve(xs filter (n=>n % p != 0))
  sieve(LazyList.from(2))


primes.take(3).toList
primes.take(7).toList
