def sumOfDivisors(n: Int, currDivider: Int): Int = {
  if n <= 0 then throw new IllegalArgumentException("n <= 0")
  else if currDivider == 0 then 0
  else{
    if(n % currDivider == 0 && currDivider < n) currDivider + sumOfDivisors(n, currDivider - 1)
    else sumOfDivisors(n, currDivider - 1)
  }
}

def isPerfect(n: Int): Boolean = n == sumOfDivisors(n,n)

isPerfect(28)
isPerfect(10)
isPerfect(-1)
