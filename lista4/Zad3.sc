
enum WeekDay:
  case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

def weekDayToString(day: WeekDay): String =
  day match
    case WeekDay.Monday => "Poniedziałek"
    case WeekDay.Tuesday => "Wtorek"
    case WeekDay.Wednesday => "Środa"
    case WeekDay.Thursday => "Czwartek"
    case WeekDay.Friday => "Piątek"
    case WeekDay.Saturday => "Sobota"
    case WeekDay.Sunday => "Niedziela"


def nextDay(day: WeekDay): WeekDay =
  day match
    case WeekDay.Monday => WeekDay.Tuesday
    case WeekDay.Tuesday => WeekDay.Wednesday
    case WeekDay.Wednesday => WeekDay.Thursday
    case WeekDay.Thursday => WeekDay.Friday
    case WeekDay.Friday => WeekDay.Saturday
    case WeekDay.Saturday => WeekDay.Sunday
    case WeekDay.Sunday => WeekDay.Monday


val currentDay = WeekDay.Monday
val next = nextDay(currentDay)
val dayString = weekDayToString(currentDay)
