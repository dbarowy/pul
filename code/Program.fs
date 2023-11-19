open Evaluator
open System
open Parser

[<EntryPoint>]
let main args =
    // Verify user provided only one file path
    if Array.length args <> 1 then 
        printfn "Usage: dotnet run <file with program>"
        exit 1

    // Save provided file path
    let file = args[0]

    // Try to open file
    let text =
        try
            IO.File.ReadAllText file
        with
            // Error can't open file
            |_ ->   printfn "Error: Can't open provided file path" 
                    exit 1
    
    match parse text with
    | Some ast ->
        printfn "AST:\n %A" ast // Print AST
        let svg = eval ast
        printfn "Evaluation:\n %A" svg // Print evaluation
        0
    | None ->
        printfn "Error while parsing. Invalid program."
        1