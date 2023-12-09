open Evaluator
open System
open Parser

[<EntryPoint>]
let main (args: string array) =
    // Verify user provided only one file path
    if Array.length args <> 1 then 
        printfn "Usage: \n  dotnet run <path to file with program>"
        exit 1

    // Save provided file path
    let file: string = args[0]

    // Try to open file
    let text: string =
        try
            IO.File.ReadAllText file
        with
            // Error can't open file
            |_ ->   printfn "Error: Can't open provided file path" 
                    exit 1
    
    match parse text with
    | Some (ast: AST.InputSchedule) ->
        printfn "\nAST:\n %A" ast // Print AST
        printfn "\n --------------------------------------------------------------------- \n"
        let svg = eval ast
        printfn "EVALUATION:\n %A \n" svg // Print evaluation
        0
    | None ->
        printfn "Error while parsing. Invalid program."
        1