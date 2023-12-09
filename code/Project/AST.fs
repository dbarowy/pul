module AST

open System

type TimeRange = {startTime: DateTime; endTime: DateTime}

type Event =
| Offer of string * string List * TimeRange * int
| Request of string * string List * TimeRange

type InputSchedule = Event list