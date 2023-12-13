let (><) list1 list2 f =
  let rec map list1 list2  =
    match list1, list2 with
    | h1::t1, h2::t2 -> f h1 h2 :: map t1 t2
    | _, _ -> []
  in
  map list1 list2;;

let add x y = x + y;;
let sub x y = x - y;;

let list1 = [1; 2; 3];;
let list2 = [4; 5; 6];;

let result1 =  (list1 >< list2) add;;

let list3 = [1;2;3;4;5];;
let list4 = [1;1;1;1;1;1;1;1];;

let result2 = (list3 >< list4) sub;;

let list5 = [1];;
let list6 = [1;2;3;4;5];;

let result3 = (list5 >< list6) add;;

let list7 = [];;
let list8 = [1;2;3];;

let result4 = (list7 >< list8) sub;;

let list9 = [];;
let list10 = [];;

let result5 = (list9 >< list10) add;;
