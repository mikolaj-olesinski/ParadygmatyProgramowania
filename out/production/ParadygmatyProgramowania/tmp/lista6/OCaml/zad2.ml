type 'a tree = Empty | Node of 'a * (unit -> 'a tree) * (unit -> 'a tree);;

let exampleTree1 =
  Node(1,
       (fun () -> Node(2,
                       (fun () -> Node(4, (fun () -> Empty), (fun () -> Empty))),
                       (fun () -> Node(5, (fun () -> Empty), (fun () -> Empty)))
                      )),
       (fun () -> Node(3, (fun () -> Empty), (fun () -> Empty)))
      );;


(*   1
    /\
   2 3    IN-ORDER = 4 2 5 1 3
  /\
 4 5
*)


let exampleTree2 =
  Node(1,
       (fun () -> Node(2,
                       (fun () -> Node(4, (fun () -> Empty), (fun () -> Empty))),
                       (fun () -> Node(5,
                                       (fun () -> Node(6, (fun () -> Empty), (fun () -> Empty))),
                                       (fun () -> Node(7, (fun () -> Empty), (fun () -> Empty)))
                                      )
                       )
                      )),
       (fun () -> Node(3, (fun () -> Empty), (fun () -> Empty)))
      );;

(*
      1
     /\
    2  3
   /\
  4 5       IN-ORDER 4 2 6 5 7 1 3
    /\
   6 7

*)


let exampleTree3 = Node (1, (fun() -> Empty),
                         (fun() -> Node (2, (fun()-> Empty), (fun()->Empty)
                                        )));;
  (* 1
    /\
     2 *)


let exampleTree4 = Node (1, (fun () -> Empty), (fun () -> Empty));;
    (* 1 IN-ORDER 1 *)

let exampleTree5 = Empty ;;



let rec treeFoldL f acc tree =
  match tree with
  | Empty -> acc
  | Node (value, leftChild, rightChild) ->
      let leftAcc = treeFoldL f acc (leftChild ()) in
      treeFoldL f (f leftAcc value) (rightChild ())
;;

let x = treeFoldL (fun acc value -> acc + value) 0 exampleTree1;;

let a = treeFoldL (fun acc value -> acc ^ string_of_int value) "" exampleTree1;;

let b = treeFoldL (fun acc value -> acc ^ string_of_int value) "" exampleTree2;;

let c = treeFoldL (fun acc value -> acc ^ string_of_int value) "" exampleTree3;;

let d = treeFoldL (fun acc value -> acc ^ string_of_int value) "" exampleTree4;;

let e = treeFoldL (fun acc value -> acc ^ string_of_int value) "" exampleTree5;


