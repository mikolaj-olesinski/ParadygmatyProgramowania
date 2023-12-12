
type Person = (String, String, Int, String, Int) // name, sunrname, age, gender, shoeSize

type Partnership = (Person, Person)

val personA: Person = ("Adam", "Kowalski", 25, "Male", 45)
val personB: Person = ("Karolina", "Nowak", 40, "Female", 39)

val partnership: Partnership = (personA, personB)

def youngerAge(partnership: Partnership): Person =
  if partnership(0)(2) < partnership(1)(2) then partnership(0) else partnership(1)

def biggerShoeSize(partnership: Partnership): Person =
  if partnership(0)(4) > partnership(1)(4) then partnership(0) else partnership(1)

def longerName(partnership: Partnership): Person =
  if partnership(0)(0).length > partnership(1)(0).length then partnership(0) else partnership(1)

youngerAge(partnership)
biggerShoeSize(partnership)
longerName(partnership)
