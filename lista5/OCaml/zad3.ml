exception IllegalArgumentException of string

let rec stirling n m =
  if n > 0 && m > 0 then
    match (n, m) with
    | (0, 0) -> 1
    | (_, 1) -> 1
    | (_, 0) -> 0
    | (a, b) when a = b -> 1
    | _ -> stirling (n - 1) (m - 1) + m * stirling (n - 1) m
  else
    raise (IllegalArgumentException "n and m must be positive")

let lazyStirling = lazy (stirling 32 16);;

print_endline "start";;

let a = Lazy.force lazyStirling