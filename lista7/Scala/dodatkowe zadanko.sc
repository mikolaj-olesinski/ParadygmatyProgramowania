def modifiedPascalI(n: Int): Array[Int] = {
  if n < 0 then throw new IllegalArgumentException("n < 0")
  val pascal = new Array[Int](2*n + 1)
  pascal(0) = 1
  pascal(1) = 1

  for (i <- 1 to n)
    var prev = 1
    var prev2 = 1

    for (j <- 2 until 2*i - 1)
      val current = pascal(j)

      pascal(j) = pascal(j) + prev + prev2

      prev = prev2
      prev2 = current

    pascal(2*i - 1) = 1
    pascal(2*i) = 1

  pascal
}

modifiedPascalI(4)


let sieve n =
  let tab = Array.make (n+1) true in
  let rec loop i =
  if i <= n then
    if tab.(i) then
      let rec loop2 j =
        if j <= n then
          if j mod i = 0 && j <> i then
            tab.(j) <- false;
          loop2 (j+1)
        else
          loop (i+1)
      in
      loop2 (i+1)
    else
      loop (i+1)
  in
  loop 2;
  let rec loop3 i =
    if i <= n then
      if tab.(i) then
        i :: loop3 (i+1)
      else
        loop3 (i+1)
    else
      []
  in
  loop3 2
