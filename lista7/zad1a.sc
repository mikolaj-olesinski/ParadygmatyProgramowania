def modifiedPascalF(n: Int): List[Int] = {
    if n < 0 then throw new IllegalArgumentException("n < 0")
    else if n == 0 then List(1)
    else
      val prev = modifiedPascalF(n - 1)
      val next = (prev zip prev.tail).map { case (x, y) => if n % 2 == 0 then x + y else x - y }
      1 :: next ::: List(1)

}

modifiedPascalF(0)
modifiedPascalF(1)
modifiedPascalF(2)
modifiedPascalF(3)
modifiedPascalF(4)
modifiedPascalF(5)
modifiedPascalF(6)
modifiedPascalF(7)


modifiedPascalF(-1)


