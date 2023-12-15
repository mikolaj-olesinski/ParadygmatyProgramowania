def modifiedPascalF(n: Int): List[Int] = {
  if (n < 0) throw new IllegalArgumentException("n < 0")
  else if (n == 0) List(1)
  else {
    val prev = modifiedPascalF(n - 1)

    def help(list: List[Int], n :Int): List[Int] = {
      list match {
        case x :: Nil => List(1)
        case x :: xs => if n % 2 == 0 then x + xs.head :: help(xs, n) else x - xs.head :: help(xs, n)
        case _ => Nil
      }
    }

    1 :: help(prev, n)
    }
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




