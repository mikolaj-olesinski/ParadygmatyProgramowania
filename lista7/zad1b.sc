def modifiedPascalI(n: Int): Array[Int] = {
  val pascal = new Array[Int](n + 1)

  for (i <- 0 to n) {
    pascal(0) = 1
    for (j <- 1 until i) {
      if (i % 2 == 0) {
        pascal(j) = pascal(j) - pascal(j - 1)
      } else {
        pascal(j) = pascal(j) + pascal(j - 1)
      }
    }
    pascal(i) = 1
  }

  pascal
}

modifiedPascalI(5)