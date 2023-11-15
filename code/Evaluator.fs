module Evaluator

open Graph
open AST

// TODO: A function that takes two events and returns true if ALL of the following are true
//  1) first is an Offer
//  2) second is a Request
//  3) offer can fulfill request
// let match (offer: Event) (request: Event): boolean = 


// Recursively construct a graph with only offer nodes
let rec constructOffersGraph (g: Graph<Event, int>) (input: InputSchedule): Graph<Event, int> =
    // Recursiely add elements of input to g
    match input with
    | []        -> g
    | x::xs     ->
        match x with
        | Offer (_,_,_,_,_)-> constructOffersGraph (Graph.addVertex x g |> snd) (xs)
        | Request (_,_,_,_)-> constructOffersGraph (g) (xs)

// Recursively add request nodes and their edges to graph
let rec addRequests (g: Graph<Event, int>) (input: InputSchedule): Graph<Event, int> =
    // Recursiely add elements of input to g
    match input with
    | []        -> g
    | x::xs     ->
        match x with
        | Offer (_,_,_,_,_)-> addRequests (g) (xs)
        | Request (_,_,_,_)-> 
            addRequests 
                (
                    // Add request node
                    Graph.addVertex x g |> snd

                    // Find offer nodes with matching constraints

                    // Add edge if offer matches request
                ) 
                (xs)

let eval (input: InputSchedule): Graph<Event, int> =
    // Construct graph
    let g = Graph.empty
    let offers = constructOffersGraph g input
    //printf "Parsed graph: %A\n\n\n\n" offers
    let offersAndRequests = addRequests offers input
    printf "Parsed graph: %A\n\n\n\n" offersAndRequests
    offersAndRequests

    // Run Ford-Fulkerson

    // Return pretty print solution



// GRAPH USAGE EXAMPLE

Graph.empty
  |> Graph.addVertex "Abe" // Returns identifier of vertex 1 and graph
  |> (fun (v1,g) -> 
    (Graph.addVertex "Bob" g) // Returns identifier of vertex 2 and graph
    |> (fun (v2,g) -> 
      (Graph.addEdge 0 v1 v2 100 g))) // Add edge with priority between v1 and v2 and label to graph g
  |> snd
  |> printf "Parsed graph: %A\n\n"