module GraphCreationFunctions

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
let timeMatchHelper(timeRange1: TimeRange)(timeRange2: TimeRange): bool =
    let time1start: DateTime = timeRange1.startTime
    let time1end: DateTime = timeRange1.endTime
    let time2start: DateTime = timeRange2.startTime
    let time2end: DateTime = timeRange2.endTime
    (time1start <= time2end) && (time1end >= time2start)

// A function that takes takes an Offer and a Request and returns true of the Offer can fulfill the Request
let fulfill (offer: Event) (request: Event): bool = 
    match offer,request with
    // Input was an Offer and Request
    | Offer (_,loc1: List<string>,timeRange1: TimeRange,_),Request (_,loc2: List<string>,timeRange2: TimeRange) ->
        // Booleans for whether each field matches
        let locMatch: bool = locMatchHelper (List.head loc1) loc2
        let timeMatch: bool = timeMatchHelper timeRange1 timeRange2

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
let rec addFullfillEdges (idRequest: int) (offerList: Vertex<Event, int> list) (g: Graph<Event,int>): Graph<Event,int> =
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
        if (fulfill (Graph.vertexData o) request) then 
            let newGraph: Graph<Event,int> = (Graph.addEdge 0 idOffer idRequest 1 g) |> snd
            let newGraph2: Graph<Event,int> = (Graph.addEdge 0 idRequest idOffer 0 newGraph) |> snd
            (addFullfillEdges idRequest os newGraph2) 
        else (addFullfillEdges idRequest os g)

// Recursive function that adds edges between the provided source ID and all offers contained in the offer list to graph g.
// New edge contains the weight of the seat capacity of the offer.
let rec addSourceToOfferEdges (idSource: int) (offerList: Vertex<Event, int> list) (g: Graph<Event,int>): Graph<Event,int> =
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
            | Offer (_,_,_,seatsNum) -> seatsNum
            | _ -> -1
        // Add a new edge between the source and this offer
        let newGraph1: Graph<Event,int> = (Graph.addEdge 0 idSource idOffer seats g) |> snd
        let newGraph2: Graph<Event,int> = (Graph.addEdge 0 idOffer idSource 0 newGraph1) |> snd
        addSourceToOfferEdges idSource os newGraph2

// A function which adds an edge with capacity 1 between every request and the provided sink node
let addRequestToSinkEdges (sinkId: int)  (g: Graph<Event,int>): Graph<Event,int> =
    // Save the list of all verticies
    let vertexList: Vertex<Event,int> list = g |> snd
    let rec addOneEdge (vertexList: ((int * Event) * 'a) list) graph = 
        match vertexList with
        // Base case: no more vertices to add edges between 
        | [] -> graph
        // Inductive step:
        | v::vs -> 
            // Save vertex v's data and Event
            let vertexData: int * Event = v |> fst
            let vertexId: int = vertexData |> fst
            let vertexEvent: Event = vertexData |> snd
            // Check if vertext v is an Offer or Request
            match vertexEvent with
            | Offer (_,_,_,_) ->  addOneEdge vs graph // Do nothing
            | Request (_,_,_) ->  
                // Add an edge
                let newGraph1: Graph<'b,int> = (Graph.addEdge 0 vertexId sinkId 1 graph) |> snd
                let newGraph2: Graph<'b,int> = (Graph.addEdge 0 sinkId vertexId 0 newGraph1) |> snd
                addOneEdge vs newGraph2 
    addOneEdge vertexList g


// Recursively construct a graph with only offer nodes
let rec constructOffersGraph (g: Graph<Event,int>) (input: InputSchedule): Graph<Event,int> =
    // Recursiely add elements of input to g
    match input with
    | []        -> g
    | x::xs     ->
        match x with
        | Offer (_,_,_,_)-> constructOffersGraph (Graph.addVertex x g |> snd) (xs)
        | Request (_,_,_)-> constructOffersGraph (g) (xs)

// Recursively add request nodes and their edges to graph
let rec addRequests (g: Graph<Event,int>) (input: InputSchedule) (offerList: Vertex<Event, int> list): Graph<Event,int> =
    // Recursiely add elements of input to g
    match input with
    | []        -> g
    | x::xs     ->
        match x with
        | Offer (_,_,_,_)-> addRequests (g) (xs) (offerList)
        | Request (_,_,_)-> 
            // Add request node
            let newGraph: (int * Graph<Event,int>) = Graph.addVertex x g

            // Add edge if offer matches request
            let revisedGraph: Graph<Event,int> = addFullfillEdges (newGraph |> fst) offerList (newGraph |> snd)

            // Recursive call
            addRequests (revisedGraph) (xs) (offerList)

let createGraph (input: InputSchedule): Graph<Event,int> * int =
    // Construct new empty graph
    let graphEmpty: Graph<'a,'b> = Graph.empty

    // Create and add source node
    let dummySourceOffer: Event = Offer("SOURCE",["SOURCE"],{startTime = DateTime(0); endTime = DateTime(0)},-1)
    let graphSource: Graph<Event,int> = (Graph.addVertex dummySourceOffer graphEmpty) |> snd

    // Add offer nodes to graph
    let offers: Graph<Event,int> = constructOffersGraph graphSource input
    let offersList: Vertex<Event,int> list = (snd offers)[0..((snd offers).Length-2)] // Save list of offer nodes only so we can refer to it

    // Add request nodes and appropriate edges to graph
    let offersAndRequests: Graph<Event,int> = addRequests offers input offersList

    // Add edges between source to all offers with weight of the offer's seat capacity
    let linkedSource: Graph<Event,int> = addSourceToOfferEdges 0 offersList offersAndRequests

    // Create and add sink node
    let dummySinkOffer: Event = Offer("SINK",["SINK"],{startTime = DateTime(0); endTime = DateTime(0)},-1)
    let graphSinkData: int * Graph<Event,int> = (Graph.addVertex dummySinkOffer linkedSource)
    let sinkId: int = graphSinkData |> fst
    let sinkGraph: Graph<Event,int> = graphSinkData |> snd

    // Add edges between all requests and sink
    let completeGraph: Graph<Event,int> = addRequestToSinkEdges sinkId sinkGraph

    // Return complete graph
    (completeGraph,sinkId)