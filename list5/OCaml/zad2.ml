let makeMemoize (funToMemoize : 'a -> 'b) : 'a -> 'b =
let memoTable = Hashtbl.create 10 in
  fun x ->
    match Hashtbl.find_opt memoTable x with
    | Some result -> result
    | None ->
        let result = funToMemoize x in
        Hashtbl.add memoTable x result;
        result ;;


let expensiveFunction n =
  n * n;;


let memoizedFunction = makeMemoize (expensiveFunction);;

print_endline "start";;

let a = expensiveFunction 5;;
let b = expensiveFunction 5;;