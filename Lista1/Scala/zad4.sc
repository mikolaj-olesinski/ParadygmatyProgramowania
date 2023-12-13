def insert[T](current_list: List[T], new_value: T, index: Int): List[T] = {
  if(index <= 0) throw new IllegalArgumentException("index <= 0")
  else if (index == 1 || current_list == Nil) {
    new_value :: current_list
  } else {
    current_list.head :: insert(current_list.tail, new_value, index - 1)
  }
}


val list1 = List(1, 2, 4, 5)
val result = insert(list1, 3, 321)

val list2= List(1, 2, 3, 4, 5)
val result1 = insert(list2, 10, 321)

val list3= List(1, 2, 3, 4, 5)
val result3 = insert(list3, 10, -1)
