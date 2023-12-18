let sieve n =
  if n < 0 then raise (Invalid_argument "n < 0")
  else
    let tab = Array.make (n) true in
    let size = ref 0 in

    for i = 2 to n - 1 do
      if tab.(i) then
        let j = ref (i * i) in
        while !j < n do
          tab.(!j) <- false;
          j := !j + i;
        done;
        size := !size + 1;
    done;

    let result = Array.make (!size) 0 in
    let resultPointer = ref 0 in

    for i = 2 to n - 1 do
      if tab.(i) then
        begin
          result.(!resultPointer) <- i;
          resultPointer := !resultPointer + 1;
        end
    done;

    result
;;

let a = sieve 100 ;;
Array.length a ;;

let b = sieve 0 ;;
let c = sieve (-1);;