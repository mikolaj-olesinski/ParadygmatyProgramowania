def sumProd(s: Int, e: Int): (Int, Int) = {
  if (s > e) {
    throw new IllegalArgumentException("s > e")
  } else if (s == e) (0, 0)
    else if (s == e - 1) (s, e - 1)
    else {
    val (sum, multiplication) = sumProd(s + 1, e)
    (s + sum, s * multiplication)

  }
}

sumProd(5, 5)
sumProd(3,4)
sumProd(1, 5)
sumProd(-3, -1)
sumProd(5, 1)
sumProd(-2, -5)

