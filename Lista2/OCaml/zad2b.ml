let split3Tail list =
  let rec split3TailHelp list acc =
    match (list, acc) with
    | (p1 :: p2 :: p3 :: tail, (part1, part2, part3)) ->
      split3TailHelp tail (p1 :: part1, p2 :: part2, p3 :: part3)
    | _ -> acc
    in
  split3TailHelp list ([], [], []);;




let zwykla_lista = [1; 2; 3; 4; 5; 6; 7; 8; 9];;
let (zwykla_1, zwykla_2, zwykla_3) = split3Tail zwykla_lista;;


let niePodzielna_przez3 = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"];;
let (niePodzielna_1, niePodzielna_2, niePodzielna_3) = split3Tail niePodzielna_przez3;;


let krotka = [1; 2];;
let (krotka_1, krotka_2, krotka_3) = split3Tail krotka;;


let pusta = [];;
let (pusta_1, pusta_2, pusta_3) = split3Tail pusta;;