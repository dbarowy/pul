module Evaluator

open Graph
open AST

let timeMatchHelper(hr1: HourRange)(hr2: HourRange): bool =
    match hr1.startHour,hr1.endHour, hr2.startHour,hr2.endHour with 
    |a,-1,c,-1 -> a = c
    |a,b,c,-1 -> (a<=c) && (c<=b) 
    |a,-1,c,d -> (c<=a) && (a<=d)
    |a,b,c,d  -> (a <= d) && (b >= c)

let dateMatchHelper(date1: DateRange)(date2:DateRange): bool = 
    match 

// TODO: A function that takes takes an Offer and a Request and returns true of the Offer can fulfill the Request
let fulfill (offer: Event) (request: Event): bool = 
    match offer,request with
    // Input was an Offer and Request
    | Offer (_,loc1,time1,date1,_),Request (_,loc2,time2,date2) ->
        // Booleans for whether each field matches
        let locMatch = ((List.head loc1) = (List.head loc2))
        let timeMatch = timeMatchHelper (time1) (time2)
        let dayMatch = ((List.head date1).day = (List.head date2).day)
        let monthMatch = ((List.head date1).month = (List.head date2).month)

        // Offer fulfills request if ALL fields match
        locMatch && timeMatch && dayMatch && monthMatch
    // Input was not an Offer and Request
    | _ -> false

// A recursive function that takes
//  1) an ID for a request vertext
//  2) a list of Offer vertices in the graph below
//  3) a graph
// And adds edges between the vertex with the provided ID and any offers in the list/graph that fulfill the request
// Returns the new graph
let rec addFullfillEdges (idRequest: int) (offerList: Vertex<Event,int*int> list) (g: Graph<Event,int*int>): Graph<Event,int*int> =
    match offerList with
    // Base case: No more offers to check
    | [] -> g
    // Inductive step: check if first offer in offerList fulfills id
    | o::os ->
        // Get the event corresponding to id
        let request: Event = (Graph.vertexData (Graph.getVertex idRequest g))
        // Get the id of the offer we are comparing to
        let idOffer: int = (Graph.vertexId o)
        // If offer fulfills request add an edge otherwise just return the original graph
        if (fulfill (Graph.vertexData o) request) then ((Graph.addEdge 0 idOffer idRequest (0,1) g) |> snd) else g


// Recursively construct a graph with only offer nodes
let rec constructOffersGraph (g: Graph<Event,int*int>) (input: InputSchedule): Graph<Event,int*int> =
    // Recursiely add elements of input to g
    match input with
    | []        -> g
    | x::xs     ->
        match x with
        | Offer (_,_,_,_,_)-> constructOffersGraph (Graph.addVertex x g |> snd) (xs)
        | Request (_,_,_,_)-> constructOffersGraph (g) (xs)

// Recursively add request nodes and their edges to graph
let rec addRequests (g: Graph<Event,int*int>) (input: InputSchedule) (offerList: Vertex<Event,int*int> list): Graph<Event,int*int> =
    // Recursiely add elements of input to g
    match input with
    | []        -> g
    | x::xs     ->
        match x with
        | Offer (_,_,_,_,_)-> addRequests (g) (xs) (offerList)
        | Request (_,_,_,_)-> 
            // Add request node
            let newGraph: (int * Graph<Event,int*int>) = Graph.addVertex x g

            // Add edge if offer matches request
            let revisedGraph = addFullfillEdges (newGraph |> fst) offerList (newGraph |> snd)

            // Recursive call
            addRequests (revisedGraph) (xs) (offerList)

let eval (input: InputSchedule): Graph<Event,int*int> =
    // Construct new empty graph
    let g = Graph.empty

    // Add offer nodes to graph
    let offers = constructOffersGraph g input
    //printf "Parsed graph: %A\n\n\n" offers
    let offersList: Vertex<Event,int*int> list = snd offers // Save list of offer nodes only so we can refer to it
    printf "Offers:\n %A\n\n\n" offersList

    // Add request nodes and appropriate edges to graph
    let offersAndRequests = addRequests offers input offersList
    printf "Parsed graph:\n %A\n\n\n" offersAndRequests

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