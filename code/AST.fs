module AST

type Date = { month: int; day: int }

type Event =
| Offer of string * string List * int List * Date List * int
| Request of string * string List * int List * Date List

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