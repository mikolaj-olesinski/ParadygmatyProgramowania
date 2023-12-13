let rec sumProd s e =
  if s > e then
    failwith "invalid numbers"
  else if s = e then
    (0, 0)
  else if s = e - 1 then
    (s, e - 1)
  else
    let (sum, multiplication) = sumProd (s + 1) e in
    (s + sum, s * multiplication)


let result = sumProd 5 10;;