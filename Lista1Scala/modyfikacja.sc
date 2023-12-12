def choice(list1: List[Int], list2: List[Int]): List[Int] = {


  if(list1.isEmpty && list2.isEmpty) List()
  else if(list1.isEmpty) list1
  else if(list2.isEmpty) list2

  else if(list1.head >= list2.head) list1.head :: choice(list1.tail, list2.tail)
  else list2.head :: choice(list1.tail, list2.tail)


}

val list1 = List(1,2,3,4,5)
val list2 = List(1,99,2,99,7,6,5,4,3)

choice(list1,list2)