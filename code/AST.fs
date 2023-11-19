module AST

type Date = { month: int; day: int }

type HourRange = {startHour: int; endHour: int}

type DateRange = {startDate: Date; endDate: Date}

type Event =
| Offer of string * string List * HourRange * DateRange* int
| Request of string * string List * HourRange * DateRange

type InputSchedule = Event list