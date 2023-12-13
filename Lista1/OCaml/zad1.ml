let reverse4 (t: 'a * 'b * 'c * 'd) : 'd * 'c * 'b * 'a =
  match t with
  (a, b, c, d) -> (d, c, b, a);;

let reversed_tuple = reverse4(1,"aha",3.5,'c')
