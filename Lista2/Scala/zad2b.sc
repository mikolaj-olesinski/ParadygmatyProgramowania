def split3Tail[T](list: List[T]): (List[T], List[T], List[T]) = {
  @scala.annotation.tailrec
  def split3TailHelp(list: List[T], acc: (List[T], List[T], List[T])): (List[T], List[T], List[T]) = {
    (list, acc) match
      case (p1::p2::p3::tail, (part1, part2, part3)) => split3TailHelp(tail, (p1::part1, p2::part2, p3::part3))
      case _ => acc
  }

  split3TailHelp(list, (Nil, Nil, Nil))
}


val zwykla_lista = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
val (zwykla_1, zwykla_2, zwykla_3) = split3Tail(zwykla_lista)

val niePodzielnaprzed3 = List("a", "b", "c", "d", "e", "f", "g", "h")
val (niePodzielna_1, niePodzielna_2, niePodzielna_3) = split3Tail(niePodzielnaprzed3)

val krotka = List(1,2)
val(krotka_1, krotka_2, krotka_3) = split3Tail(krotka)

val pusta = List()
val (pusta_1, pusta_2, pusta_3) = split3Tail(pusta)