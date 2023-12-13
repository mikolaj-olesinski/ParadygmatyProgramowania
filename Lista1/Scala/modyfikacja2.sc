def squish(lists: List[List[Int]]): List[Int] = {
  if (lists.isEmpty) List()
  else if (lists.head == List()) squish(lists.tail)
  else  lists.head.head :: squish(lists.head.tail :: lists.tail)
}

val list = List(List(1,2,3), List(4,5), List(7,8,9,10))
val list1 = List(List(), List(12,13,14,15), List(5,6,4))
val list2 = List(List(), List(), List())
val list3 = List()

squish(list)
squish(list1)
squish(list2)
squish(list3)


