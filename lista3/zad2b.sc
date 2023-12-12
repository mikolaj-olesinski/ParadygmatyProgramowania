def composites(n:Int): Seq[Int] = {

  def isComposite(number: Int): Boolean = {
    if number <= 3 then false
    else if number % 2 == 0 then true
    else if (for(i <-  List.range(3, math.sqrt(number).toInt + 1, 2);if number % i == 0 ) yield i) == Nil then false

    else true
  }

  for(i <-  List.range(2, n + 1); if isComposite(i)) yield i
}


composites(1000).length
composites(10)
composites(100)
composites(0)
composites(-1)
