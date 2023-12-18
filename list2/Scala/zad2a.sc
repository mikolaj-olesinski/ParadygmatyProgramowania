def split3Rec[T](list: List[T]): (List[T], List[T], List[T]) = {
  list match
    case p1 :: p2 :: p3 :: tail =>
      val (part1, part2, part3) = split3Rec(tail)
      (p1 :: part1, p2 :: part2, p3 :: part3)
    case _ => (Nil, Nil, Nil)
}


val zwykla_lista = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
val (zwykla_1, zwykla_2, zwykla_3) = split3Rec(zwykla_lista)

val niePodzielnaprzez3 = List("a", "b", "c", "d", "e", "f", "g", "h")
val (niePodzielna_1, niePodzielna_2, niePodzielna_3) = split3Rec(niePodzielnaprzez3)

val krotka = List(1,2)
val(krotka_1, krotka_2, krotka_3) = split3Rec(krotka)

val pusta = List()
val (pusta_1, pusta_2, pusta_3) = split3Rec(pusta)
