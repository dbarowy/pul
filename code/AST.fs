module AST

type Date = { month: int; day: int }

type HourRange = {startHour: int; endHour: int}

type DateRange = {startDate: Date; endDate: Date}

type Event =
| Offer of string * string List * HourRange * DateRange* int
| Request of string * string List * HourRange * DateRange

type InputSchedule = Event list



// OLD CODE OLD CODE OLD CODE OLD CODE 

type Coordinate = { x: int; y: int }
type Color =
| Red
| Green
| Blue
| Purple
type Line = { c1: Coordinate; c2: Coordinate; color: Color }
type Canvas = Line list

let CANVAS_SZ = 400