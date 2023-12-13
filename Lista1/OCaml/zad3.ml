
let rec sumOfDivisors n currDivider =
  if n <= 0 then
    failwith "invalid numbers"
  else if currDivider = 0 then
    0
  else
    if n mod currDivider = 0 && currDivider < n then
      currDivider + sumOfDivisors n (currDivider - 1)
    else
      sumOfDivisors n (currDivider - 1)

let isPerfect n =
   n = sumOfDivisors n n


let result2 = isPerfect 38;;
