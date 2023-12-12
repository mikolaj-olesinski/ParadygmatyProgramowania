enum Maybe[+A]:
  case Just(value: A)
  case Nothing

def safeHead[A](xs: List[A]): Maybe[A] =
  xs match
    case Nil => Maybe.Nothing
    case h :: xs => Maybe.Just(h)


val list1 = List(1, 2, 3)
val list2 = Nil
val list3 = List("a", "b", "c")

safeHead(list1)
safeHead(list2)