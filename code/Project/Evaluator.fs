module Evaluator

open Graph
open AST

open GraphCreationFunctions
open GraphMaxFlowAlgorithm

let eval (input: InputSchedule): Graph<Event,int> =
    let result: Graph<Event,int> = createGraph input
        // Example1: 0 16
        // Example2: 0 5
        // Example3: 0 9
    
    (*let mutable testArray = (Array.init (result |> fst) (fun i -> -1))
    printfn "TEST ARRAY BEFORE: %A" testArray
    let BFSresult = BFS result 0 9 testArray
    printfn "%A" BFSresult
    printfn "TEST ARRAY AFTER: %A" testArray*)

        // Example1: 0 23
        // Example2: 0 7
        // Example3: 0 13
        // Example4: 0 7
        // Example5: 0 16

    let maxFlow: int = fordFulkerson result 0 16
    printfn "MAX FLOW: %A" maxFlow

    result