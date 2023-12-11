module Parser

open AST
open Combinator
open System

// Constructs an offer from:
let offerConstructor(name: string,locationList: List<string>,timeRange: TimeRange,seats: int): Event = 
    Offer (name,locationList,timeRange,seats)

// Constructs a request from:
let requestConstructor(name: string,locationList: List<string>,timeRange: TimeRange): Event = 
    Request (name,locationList,timeRange)

// Padding parser
let pad p = pbetween pws0 p pws0

// Recursive Start
let expr, exprImpl = recparser()

// String parser
let string = pmany1 pletter |>> (fun (ds: char list) -> stringify ds)

// Parses a , then string with arbitrary spacing
let stringComma = pleft (pad string) (pad (pchar (',')))

// Locations parser that looks for at least one comma separated string
// Can handle something like:
//      Boston
//      NYC, Chicago, Boston
// Returns an array of the locations
let locations = pseq (pmany0 stringComma) (string) (fun (x: string list,y: string) -> x@[y])

// Generic number parser that returns an int
let number = pmany1 pdigit |>> (fun (ds: char list) -> stringify ds |> int)

// Parses a date of the format:
//      HH PM MM/DD/YYYY
let timeFormat1 = 
    pseq
        (pad (number))
        (pseq
            (pleft (pad ((pstr "AM") <|> (pstr "PM"))) (pad (pstr "on")))
            (pad (pseq
                    (number)
                    (pseq
                        (pright (pchar '/') (number))
                        (pright (pchar '/') (number))
                        (id)
                    )
                    (id)
            ))
            (id)
        )
        // Turn the parsed input into an F# DateTime
        (fun (hourNum: int, (hourStr: string, (month: int, (day: int, year: int)))) ->
            let strRepresentation: string = $"{hourNum} {hourStr} {month}/{day}/{year}"
            // The build in F# DateTime.TryParse function is very forgiving and will still parse most seemingly miscronstructed dates
            DateTime.TryParse(strRepresentation) |> snd
        )

// Parses just one time, i.e. "at 2am on 5/22/2023"
let oneTime = 
    pad (pright (pstr "at ") (timeFormat1))|>> (fun (time1: DateTime) -> {startTime = time1; endTime = time1})
    
// Parses two times, i.e. "at 2am on 5/22/2023 to 10am on 5/23/2023"
let twoTimes = 
    pseq 
        (pad (pright (pstr "at ") (timeFormat1)))
        (pad (pright (pstr "to ") (timeFormat1)))
        (fun (time1: DateTime, time2: DateTime) -> {startTime = time1; endTime = time2})

// Parser looking for two times first otherwise one time
let timeParser = twoTimes <|> oneTime

// Offer parser
let offer = 
    pseq
        (pad (pright (pstr "Offer: ") (string)))
        (pseq 
            (pad (pright (pstr "to ") (string)))
            (pseq
                (pad (timeParser))
                (pad (pbetween (pstr "with ") (number) ((pstr " seats") <|> (pstr " seat"))))
                id
            )
            id
        )
        (fun (name: string, (location: string, (timeRange, seats: int))) ->
            offerConstructor (name,[location],timeRange, seats)
        )

// Request parser
let request = 
    pseq
        (pad (pright (pstr "Request: ") (string)))
        (pseq 
            (pad (pright (pstr "to ") (locations)))
            (pad (timeParser))
            id
        )
        (fun (name: string, (location: string list, timeRange: TimeRange)) ->
            requestConstructor (name,location,timeRange)
        )

// An event is a request or offer
let event = pad offer <|> request

// Recursive End: a program can have many events
exprImpl :=
    pmany1 (event |>> (fun l -> [l])) |>> List.concat

let grammar = pleft expr peof

let parse (input: string) : InputSchedule option =
    let i: Input = prepare input
    match grammar i with
    | Success(ast: Event list, _) -> Some ast
    | Failure(_,_) -> None