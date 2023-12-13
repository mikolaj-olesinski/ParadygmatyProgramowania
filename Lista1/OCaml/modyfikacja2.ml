let rec squish lists =
  if lists = [] then
    []
  else if List.hd lists = [] then
    squish (List.tl lists)
  else List.hd (List.hd lists) :: squish (List.tl (List.hd lists) :: List.tl lists)

let list1 = [[1;2;3];[4;5];[6;7;8;9]]
let list2 = [[];[4;5];[6;7;8;9]]
let list3 = [[];[];[]]
let list4 = []

let result1 = squish list1
let result2 = squish list2
let result3 = squish list3
let result4 = squish list4
