let rec cutOut list a b =
  let rec cutOutAfterConditions list a b =
    match (a, b) with
    | (0, -1) -> []
    | (0, _) -> List.hd list :: cutOut (List.tl list) 0 (b - 1)
    | (_) -> cutOut (List.tl list) (a - 1) (b - 1)
  in

  match (list, a < 0,(b < 0 || a > b)) with
  | ([], _, _) -> []
  | (_, _, true) -> []
  | (_, true, _) -> cutOutAfterConditions list 0 b
  | (_) -> cutOutAfterConditions list a b;;


let cut_out15 list = cutOut list 1 5;;
let cut_out22 list = cutOut list 2 2;;
let cut_out_46 list = cutOut list (-4) 6;;
let cut_out_3_5 list = cutOut list (-3) (-5);;
let cut_out910 list = cutOut list 9 10;;


let zwykla = [0; 1; 2; 3; 4; 5; 6; 7];;
let jedno_elementowa = [1];;
let pusta = [];;


let result1 = cut_out15 zwykla;;
let result2 = cut_out15 jedno_elementowa;;
let result3 = cut_out15 pusta;;

let result4 = cut_out22 zwykla;;

let result5 = cut_out_46 zwykla;;

let result6 = cut_out_3_5 zwykla;;

let result7 = cut_out910 zwykla;;