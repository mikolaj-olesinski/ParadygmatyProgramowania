def cutOut[T](list: List[T], a: Int, b: Int):List[T] = {
  def cutOutAfterConditions(list: List[T], a: Int, b: Int):List[T] = {
    (a, b) match
    case (0, - 1) => Nil
    case (0, _) => list.head :: cutOut (list.tail, 0, b - 1);
    case _ => cutOut (list.tail, a - 1, b - 1)
  }

  (list, a < 0, (b < 0 || a > b)) match
    case (Nil, _, _) => Nil
    case (_, _, true) => Nil
    case (_, true, _) => cutOutAfterConditions(list, 0, b)
    case _ => cutOutAfterConditions(list, a, b)
}

def cutOut15[A](list: List[A]): List[A] = cutOut(list, 1, 5)
def cutOut22[A](list: List[A]): List[A] = cutOut(list, 2, 2)
def cutOut_46[A](list: List[A]): List[A] = cutOut(list, -4, 6)
def cutOut_3_5[A](list: List[A]): List[A] = cutOut(list, -3, -5)
def cutOut910[A](list: List[A]): List[A] = cutOut(list, 9, 10)

val zwykla= List(0,1,2,3,4,5,6,7)
val jednoElementowa = List(1)
val pusta = List()

val result1 = cutOut15(zwykla)
val result2 = cutOut15(jednoElementowa)
val result3 = cutOut15(pusta)

val result4 = cutOut22(zwykla)

val result5 = cutOut_46(zwykla)

val result6 = cutOut_3_5(zwykla)

val result7 = cutOut910(zwykla)
