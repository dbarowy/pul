module ParserOLD

open AST
open Combinator
open System

let r = new Random()

(*
 *   <expr> ::= <line><expr>
 *           |  repeat<n><line><expr>
 *           |  <empty>
 *   <line> ::= <color> line
 *      <n> ::= (is any positive integer)
 *  <color> ::= red | green | blue | purple
 *)

let randomLine (color: Color) : Line =
    let a = { x = r.Next CANVAS_SZ; y = r.Next CANVAS_SZ}
    let b = { x = r.Next CANVAS_SZ; y = r.Next CANVAS_SZ}
    { c1 = a; c2 = b; color = color }

let pad p = pbetween pws0 p pws0

let expr, exprImpl = recparser()

let color =
    (pstr "red" |>> (fun _ -> Red)) <|>
    (pstr "green" |>> (fun _ -> Green)) <|>
    (pstr "blue" |>> (fun _ -> Blue)) <|>
    (pstr "purple" |>> (fun _ -> Purple))

let line =
    pleft
        (pad color)
        (pad (pstr "line"))
    |>> randomLine

let n = pmany1 pdigit |>> (fun ds -> stringify ds |> int)

let repeat =
    pseq
        (pright (pad (pstr "repeat")) (pad n))
        line
        (fun (i, l) ->
            [0..i-1] |> List.map (fun _ -> randomLine l.color)
        )
exprImpl :=
    pmany1 (
        line |>> (fun l -> [l]) <|>
        repeat
    ) |>> List.concat

let grammar = pleft expr peof

let parse (input: string) : Canvas option =
    let i = prepare input
    match grammar i with
    | Success(ast, _) -> Some ast
    | Failure(_,_) -> None