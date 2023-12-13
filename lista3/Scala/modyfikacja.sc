def composites(n:Int): List[Int] = {


  def filter(f: Int => Boolean, xs: List[Int]): List[Int] = {
    xs match
      case h :: t => if f(h) then h :: filter(f, t) else filter(f, t)
      case _ => Nil
  }

  def isComposite(number: Int): Boolean = {
    if number <= 3 then return false
    else if number % 2 == 0 then return true

    for(i <-  List.range(3, math.sqrt(number).toInt + 1, 2)) if number % i == 0 then return true

    false
  }

  filter(isComposite, List.range(1, n + 1))
}

composites(1000).length
composites(10)
composites(100)
composites(0)
composites(-1)