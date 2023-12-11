module Evaluator

open Graph
open AST

open GraphCreationFunctions
open GraphMaxFlowAlgorithm
open GraphPrettyPrint

let eval (input: InputSchedule) =
    // Make the graph of input
    let graph: Graph<Event,int> = (createGraph input) |> fst
    let sinkID: int= (createGraph input) |> snd

    // Run Ford-Fulkerson/Edmonds-Karp
    let resultGraph: Graph<Event,int> = (fordFulkerson graph 0 sinkID) |> snd

    // Construct the string representation of the flow/maximal matches after Ford-Fulkerson/Edmonds-Karp
    prettyPrint sinkID resultGraph