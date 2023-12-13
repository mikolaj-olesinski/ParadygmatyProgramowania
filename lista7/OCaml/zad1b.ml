let modifiedPascalI n =
  if n < 0 then raise (Invalid_argument "n < 0")
  else
    let pascal = Array.make (n + 1) 0 in
    pascal.(0) <- 1;
    for i = 1 to n do
      let prev = ref 1 in
      for j = 1 to i - 1 do
        let current = pascal.(j) in
        if i mod 2 = 0 then pascal.(j) <- !prev + current
        else pascal.(j) <- !prev - current;
        prev := current;
      done;
      pascal.(i) <- 1;
    done;
    pascal;;