module Parser

open AST
open Combinator
open System

// Constructs an offer from:
let offerConstructor((name,location,time,date,seats): string * string List * int List * Date * int): Event = 
    Offer (name,location,time,date,seats)

// Constructs a request from:
let requestConstructor((name,location,time,date): string * string List * int List * Date): Event = 
    Request (name,location,time,date)

// TODO: Do we actually need this function for pseq?
// Pass along function
let passAlong a = a

// Padding parser
let pad p = pbetween pws0 p pws0

// Recursive Start
let expr, exprImpl = recparser()

// String parser
let string = pmany1 pletter |>> (fun ds -> stringify ds)

// TODO: String list parser to get location list:
// Can handle something like:
//      Boston
//      NYC, Chicago, Boston
// let stringList = 

// Number parser
let number = pmany1 pdigit |>> (fun ds -> stringify ds |> int)

// TODO: Add an hour parser that only accepts 0-23
// Replaces the generic use of number below
// let hour = 

// TODO: Add a time parser that accepts:
//      hour
//      hour-hour
// let time = 

// Date parser
let date =
    pseq
        (pleft (number) (pchar '/'))
        (number)
        (fun (m, d) ->
            { month = m; day = d}
        )

// Offer parser
let offer = 
    pseq
        (pad (pright (pstr "Offer: ") (string)))
        (pseq 
            (pad (pright (pstr "to ") (string)))
            (pseq
                (pad (pright (pstr "at ") (number)))
                (pseq
                    (pad (pright (pstr "on ") (date)))
                    (pad (pbetween (pstr "with ") (number) ((pstr " seats") <|> (pstr " seat"))))
                    (passAlong)
                )
                (passAlong)
            )
            (passAlong)
        )
        (fun (name, (location, (time, (date, seats)))) ->
            offerConstructor (name,[location],[time], date, seats)
        )

// Request parser
let request = 
    pseq
        (pad (pright (pstr "Request: ") (string)))
        (pseq 
            (pad (pright (pstr "to ") (string)))
            (pseq
                (pad (pright (pstr "at ") (number)))
                (pad (pright (pstr "on ") (date)))
                (passAlong)
            )
            (passAlong)
        )
        (fun (name, (location, (time, date))) ->
            requestConstructor (name,[location],[time], date)
        )

// An event is a request or offer
let event = pad offer <|> request

// Recursive End: a program can have many events
exprImpl :=
    pmany1 (event |>> (fun l -> [l])) |>> List.concat

let grammar = pleft expr peof

let parse (input: string) : InputSchedule option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None