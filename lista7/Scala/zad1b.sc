def modifiedPascalI(n: Int): Array[Int] = {
  if n < 0 then throw new IllegalArgumentException("n < 0")
  val pascal = new Array[Int](n + 1)
  pascal(0) = 1

  for (i <- 1 to n) {
    var prev = 1
    
    for (j <- 1 until i) {
      val current = pascal(j)
      if i % 2 == 0 then pascal(j) = prev + current
      else pascal(j) = prev - current
      
      prev = current
    }
    pascal(i) = 1
  }

  pascal
}


modifiedPascalI(0)
modifiedPascalI(1)
modifiedPascalI(2)
modifiedPascalI(3)
modifiedPascalI(4)
modifiedPascalI(5)
modifiedPascalI(6)
modifiedPascalI(7)

modifiedPascalI(-1)