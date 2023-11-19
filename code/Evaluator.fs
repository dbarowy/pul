module Evaluator

open Graph
open AST

open System

// Helper function which takes
//      1) An offer's location
//      2) A list of request locatons
// And returns true if the offer location is in the list of request locations
let rec locMatchHelper(offerLoc: String)(reqLocs: String List): bool =
    match reqLocs with
    | []    -> false
    | x::xs  -> if x = offerLoc then true else locMatchHelper offerLoc xs


// Helper function that takes
//      1) Event A's hour range
//      2) Event A's date range
//      3) Event B's hour range
//      4) Event B's date range
// And returns true if they overlap and false if they do not. Takes advantage of F# DateTime type to calculate differences
// between times.
let timeMatchHelper(h1: HourRange)(d1: DateRange)(h2: HourRange)(d2: DateRange): bool =
    let time1start = DateTime(2023, d1.startDate.month, d1.startDate.day, h1.startHour, 0, 0)
    let time1end = DateTime(2023, d1.endDate.month, d1.endDate.day, h1.endHour, 0, 0)
    let time2start = DateTime(2023, d2.startDate.month, d2.startDate.day, h2.startHour, 0, 0)
    let time2end = DateTime(2023, d2.endDate.month, d2.endDate.day, h2.endHour, 0, 0)
    (time1start <= time2end) && (time1end >= time2start)

// A function that takes takes an Offer and a Request and returns true of the Offer can fulfill the Request
let fulfill (offer: Event) (request: Event): bool = 
    match offer,request with
    // Input was an Offer and Request
    | Offer (_,loc1,hours1,dates1,_),Request (_,loc2,hours2,dates2) ->
        // Booleans for whether each field matches
        let locMatch = locMatchHelper (List.head loc1) loc2
        let timeMatch = timeMatchHelper hours1 dates1 hours2 dates2

        // Offer fulfills request if ALL fields match
        locMatch && timeMatch
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
        if (fulfill (Graph.vertexData o) request) then (addFullfillEdges idRequest os ((Graph.addEdge 0 idOffer idRequest (0,1) g) |> snd)) else (addFullfillEdges idRequest os g)


let rec addSourceToOfferEdges (idSource: int) (offerList: Vertex<Event,int*int> list) (g: Graph<Event,int*int>): Graph<Event,int*int> =
    match offerList with
    // Base case: No more offers to check
    | [] -> g
    // Inductive step: check if first offer in offerList fulfills id
    | o::os ->
        // Get the id of the offer we are comparing to
        let idOffer: int = (Graph.vertexId o)
        // Get the offer event data
        let offer: Event = (Graph.vertexData (Graph.getVertex idOffer g))
        // Get the seats field of this offer (should never hit the default case of this match)
        let seats =
            match offer with
            | Offer (_,_,_,_,seatsNum) -> seatsNum
            | _ -> -1
        // Add a new edge between the source and this offer
        addSourceToOfferEdges idSource os ((Graph.addEdge 0 idSource idOffer (0,seats) g) |> snd)

let addRequestToSinkEdges (sinkId: int)  (g: Graph<Event,int*int>): Graph<Event,int*int> =
    // Save the list of all verticies
    let vertexList = g |> snd
    let rec addOneEdge vertexList graph = 
        match vertexList with
        // Base case: no more vertices to add edges between 
        | [] -> graph
        // Inductive step:
        | v::vs -> 
            let vertexData = v |> fst
            let vertexId = vertexData |> fst
            let vertexEvent = vertexData |> snd
            match vertexEvent with
            | Offer (_,_,_,_,_) ->  addOneEdge vs graph // Do nothing
            | Request (_,_,_,_) ->  addOneEdge vs ((Graph.addEdge 0 vertexId sinkId (0,1) graph) |> snd) // Add an edge
    addOneEdge vertexList g


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
    //printfn "Entered Eval"
    // Construct new empty graph
    let graphEmpty = Graph.empty

    // Create and add source node
    let dummySourceOffer = Offer("SOURCE",["SOURCE"],{startHour = -1; endHour = -1},{startDate = {month = -1; day = -1}; endDate = {month = -1; day = -1}},-1)
    let graphSource = (Graph.addVertex dummySourceOffer graphEmpty) |> snd

    // Add offer nodes to graph
    let offers = constructOffersGraph graphSource input
    //printf "Parsed graph: %A\n\n\n" offers
    let offersList: Vertex<Event,int*int> list = (snd offers)[0..((snd offers).Length-2)] // Save list of offer nodes only so we can refer to it
    //printf "Offers:\n %A\n\n\n" offersList

    // Add request nodes and appropriate edges to graph
    let offersAndRequests = addRequests offers input offersList
    //printf "Parsed graph:\n %A\n\n\n" offersAndRequests

    // Add edges between source to all offers with weight of the offer's seat capacity
    let linkedSource = addSourceToOfferEdges 0 offersList offersAndRequests

    // Create and add sink node
    let dummySinkOffer = Offer("SINK",["SINK"],{startHour = -1; endHour = -1},{startDate = {month = -1; day = -1}; endDate = {month = -1; day = -1}},-1)
    let graphSinkData = (Graph.addVertex dummySinkOffer linkedSource)
    let sinkId = graphSinkData |> fst
    let sinkGraph = graphSinkData |> snd

    // Add edges between all requests and sink
    let completeGraph = addRequestToSinkEdges sinkId sinkGraph

    completeGraph

    // Run Ford-Fulkerson

    // Return pretty print solution