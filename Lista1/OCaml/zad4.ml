let rec insert current_list new_value index =
  if index <= 0 then
    failwith "wrong index"
  else if current_list = [] then
    new_value :: current_list
  else if index = 1 then
    new_value :: current_list
  else
    List.hd current_list :: insert (List.tl current_list) new_value (index - 1)


let new_list = insert [1;2;3] 99 2;;
