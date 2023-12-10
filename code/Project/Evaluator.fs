module Evaluator

open Graph
open AST

open GraphCreationFunctions
open GraphMaxFlowAlgorithm

let eval (input: InputSchedule): Graph<Event,int*int> =
    let result: Graph<Event,(int * int)> = createGraph input
        // Example1: 0 16
        // Example2: 0 5
        // Example3: 0 9
    let BFSresult = BFS result 0 9 (Array.init 50 (fun i -> -1))
    printfn "%A" BFSresult
    result