exception IllegalArgumentException of string

let rec stirling n m =
  if n > 0 && m > 0 then
    match (n, m) with
    | (0, 0) -> 1
    | (_, 1) -> 1
    | (_, 0) when n > 0 -> 0
    | (a, b) when a = b -> 1
    | _ -> stirling (n - 1) (m - 1) + m * stirling (n - 1) m
  else raise (IllegalArgumentException "n and m must be positive");;



let memoizedStirling n m =
  if n >= 0 && m >= 0 then
    let cache = Hashtbl.create 10 in

    let rec stirling n m =
      match Hashtbl.find_opt cache (n, m) with
      |Some result -> result
      | None ->
          let result =
            match (n, m) with
            | (0, 0) -> 1
            | (_, 1) -> 1
            | (_, 0) when n > 0 -> 0
            | (a, b) when a = b -> 1
            | _ -> stirling (n - 1) (m - 1) + m * stirling (n - 1) m
          in
          Hashtbl.add cache (n, m) result;
          result
    in
    stirling n m
  else
    raise (IllegalArgumentException "n and m must be positive");;

print_endline "start";;
print_endline (string_of_int (memoizedStirling 33 15));;
print_endline (string_of_int (stirling 33 15));;