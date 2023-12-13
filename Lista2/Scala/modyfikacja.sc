def interleave[T](list: List[T])  (otherList: List[T]) (n: Int): List[T] = {
  def interleaveHelp(list: List[T]) (otherList: List[T]) (n: Int) (currentPosition: Int): List[T] =
    (list, otherList, n == currentPosition) match
    case (_, Nil, _) => Nil
    case (Nil, _, false ) => Nil
    case (Nil, _, true) => otherList.head :: Nil
    case (_, _, false) => list.head :: interleaveHelp(list.tail) (otherList) (n) (currentPosition + 1)
    case _ => otherList.head :: interleaveHelp(list) (otherList.tail) (n) (currentPosition - 1)


  if n < 0 then interleaveHelp(list) (otherList) (0) (0)
  else interleaveHelp(list) (otherList) (n) (0)
}


val lista1 = List(0, 1, 2, 3, 4, 5, 6)
val lista2 = List(9,9,9,9,9,9,9)
val pusta = List()
val krotka = List(8,8,8)

interleave(lista1) (lista2) (3)

interleave(lista1) (lista2) (-2)

interleave(pusta) (lista2) (2)

interleave(lista1) (pusta) (1)

interleave(lista1) (krotka) (10)

interleave(lista1) (krotka) (2)





