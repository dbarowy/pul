open Evaluator
open System
open Parser

[<EntryPoint>]
let main args =
    let file = args[0]
    let text = IO.File.ReadAllText file
    match parse text with
    | Some ast ->
        let svg = eval ast
        //printfn "%A" svg
        printfn "AST:\n %A" ast // Just print AST
        0
    | None ->
        printfn "Invalid program."
        1